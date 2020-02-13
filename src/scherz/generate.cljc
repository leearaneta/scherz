(ns scherz.generate
  (:require [clojure.core.reducers :as r]
            #?(:clj [clojure.spec.alpha :as spec]
               :cljs [cljs.spec.alpha :as spec]) 
            [scherz.util :as u]
            [scherz.scale :as s]
            [scherz.gravity :as g]
            [scherz.brightness :as b]
            [scherz.chord :as c]))

(spec/def ::tension-val (spec/and number? #(<= 0 % 1)))
(spec/def ::tension-direction #{"asc" "desc"})

(spec/def ::color ::tension-val)
(spec/def ::dissonance ::tension-val)
(spec/def ::gravity ::tension-val)
(spec/def ::arc ::tension-direction)
(spec/def ::incline ::tension-direction)
(spec/def ::temper ::tension-direction)

(spec/def ::tension (spec/keys :req-un [::color ::dissonance ::gravity]
                               :opt-un [::arc ::incline ::temper]))

(spec/def ::seed int?)
(spec/def ::options (spec/keys :opt-un [:scherz.brightness/tonic
                                        :scherz.chord/type
                                        ::seed]))

(defn- apply-scores
  "Picks a chord that has the lowest combined score."
  [chords & flist]
  (let [combine-scores (fn [scores]
                         (reduce (fn [a b]
                                   (when (and (some? a) (some? b)) (+ a b)))
                                 scores))
        cost (fn [[_ score]]
               (if (nil? score) u/infinity score))]
    (->> chords
         (r/map (apply juxt flist)) ; apply each scoring function to every chord
         (r/map combine-scores) ; combine individual scores for each chord
         r/foldcat
         (map vector chords)      ; get tuples of [chord score]
         (u/min-by-coll cost)     ; choose chord(s) with lowest scores
         (map first))))

(defn- apply-incline [incline centroid chords]
  (if (some? incline)
    (let [inclinef (if (= incline "asc") pos? neg?)
          right-direction? (fn [chord]
                             (inclinef (- (u/avg (:notes chord)) centroid)))]
      (filter right-direction? chords))
    chords))

(defn- apply-temper [temper chords]
  (if (some? temper)
    (let [temperf (if (= temper "asc") pos? neg?)]
      (filter (comp temperf :temper) chords))
    chords))

(defn- chord-sets
  "Returns chords within the given scale and target color from the previous chord."
  [prev-chord target-color arc incline temper scale]
  (let [target-color (Math/round (double (* 5 target-color)))
        arcfs (cond
                  (or (= target-color 0) (= arc "asc")) [+]
                  (= arc "desc") [-]
                  :else [+ -])
        apply-arc (fn [arcf]
                    (-> (b/scale-brightness (keyword (:scale prev-chord)))
                        (+ (b/pitch->brightness (:tonic prev-chord)))
                        (arcf target-color) ; add or subtract target color
                        ; isolate amount of brightness we need from new tonic
                        (- (b/scale-brightness scale)) 
                        b/brightness->pitch ; get new tonic
                        (c/chord-set scale)))
        centroid (u/avg (:notes prev-chord))]
    (->> (mapcat apply-arc arcfs)
         (apply-incline incline centroid)
         (apply-temper temper))))

(def scale-dissonance
  "Mapping of scales to tuples of (minimum-dissonance, maximum-dissonance).
   These tuples are used to normalize dissonance between multiple scales."
  (->> s/scales
       (map (comp u/extent (partial map :dissonance) c/base-chord-sets))
       (map vector s/scales)
       (into {})))

(defn normalize-dissonance
  "Outputs a normalized dissonance value from 0 to 1 given a base dissonance value
   and a set of scales."
  [dissonance scales]
  (let [[min-vals max-vals] (apply map vector (map scale-dissonance scales)) 
        min-dissonance (apply min min-vals)
        max-dissonance (apply max max-vals)]
    (-> dissonance (- min-dissonance) (/ (- max-dissonance min-dissonance)))))

(defn generate-chords
  [scales prev tension]
  {:pre [(spec/assert (spec/* :scherz.scale/scale) scales)
         (spec/assert :scherz.chord/chord prev)
         (spec/assert ::tension tension)]}
  (let [scales (map keyword scales)
        {:keys [color dissonance gravity arc incline temper]} tension
        chords (mapcat (partial chord-sets prev color arc incline temper) scales)
        extent (u/extent (map b/pitch->brightness (:pitches prev)))
        score-color (fn [chord]
                      (-> (b/color (:extent chord) extent)
                          (/ 5)
                          (u/abs-diff color)))
        score-dissonance (fn [chord]
                           (-> (:dissonance chord)
                               (normalize-dissonance scales)
                               (u/abs-diff dissonance)))
        score-gravity (fn [chord]
                        (when-let [g (g/chord-gravity (:notes prev)
                                                      (:notes chord))]
                          (max (- gravity g) 0)))]
    (map (fn [chord] (dissoc chord :extent))
         (apply-scores chords score-color score-dissonance score-gravity))))

(defn initial-chord
  "Finds the first chord of a certain type within the given scales."
  ([scales tonic]
   {:pre [(spec/assert (spec/* :scherz.scale/scale) scales)]}
   (initial-chord scales tonic (name (first (c/possible-chord-types scales)))))
  ([scales tonic type]
   {:pre [(spec/assert (spec/* :scherz.scale/scale) scales)
          (spec/assert :scherz.brightness/tonic tonic)
          (spec/assert :scherz.chord/type type)
          (c/possible-chord-type? scales type)]}
   (dissoc (->> (map keyword scales)
                (mapcat (partial c/chord-set tonic))
                (filter (comp (partial = type) :type))
                (u/find-coll (comp (partial = 0) :inversion)))
           :extent)))

(defn- next-chord
  "Finds the next chord of a progression within the given scales.
   Seed is used if there is a tie between possible chord choices."
  ([scales prev tension] (next-chord 0 scales prev tension))
  ([seed scales prev tension]
   (let [chords (generate-chords scales prev tension)]
     (get (vec chords)
          (mod seed (count chords))))))

(defn generate-progression
  "Repeatedly calls next-chord to generate a chord progression."
  ([scales tensions] (generate-progression scales tensions nil))
  ([scales tensions options]
   {:pre [(spec/assert (spec/* :scherz.scale/scale) scales)
          (spec/assert (spec/* ::tension) tensions)]}
   (let [{:keys [tonic type seed]
          :or {tonic "C"
               type (name (first (c/possible-chord-types scales)))
               seed 0}} options]
     (reductions (partial next-chord (int seed) scales)
                 (initial-chord scales tonic type)
                 tensions))))

(let [scales [:major :lydian]
      initial-chord (initial-chord scales "C" "M7")
      tension {:color 0.35 :dissonance 0.25 :gravity 0.5}]
  (generate-chords scales initial-chord tension))
