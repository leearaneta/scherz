(ns scherz.generate
  (:require [clojure.core.reducers :as r]
            #?(:clj [clojure.spec.alpha :as spec]
               :cljs [cljs.spec.alpha :as spec]) 
            [scherz.util :as u]
            [scherz.scale :as s]
            [scherz.gravity :as g]
            [scherz.brightness :as b]
            [scherz.chord :as c]))

(spec/def ::force-val (spec/and number? #(<= 0 % 1)))
(spec/def ::force-direction #{"asc" "desc"})

(spec/def ::color ::force-val)
(spec/def ::dissonance ::force-val)
(spec/def ::gravity ::force-val)
(spec/def ::arc ::force-direction)
(spec/def ::incline ::force-direction)
(spec/def ::temper ::force-direction)

(spec/def ::force (spec/keys :req-un [::color ::dissonance ::gravity]
                             :opt-un [::arc ::incline ::temper]))

(spec/def ::seed int?)
(spec/def ::options
  (spec/keys :opt-un [:scherz.brightness/tonic ::seed ::dissonance]))

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
       (map (comp u/extent (partial map :dissonance) c/c-chords))
       (map vector s/scales)
       (into {})))

(defn normalize-dissonance
  "Returns a function that outputs a normalized dissonance value from 0 to 1 
   given a base dissonance value and a set of scales."
  [scales]
  (let [[min-vals max-vals] (apply map vector (map scale-dissonance scales)) 
        min-dissonance (apply min min-vals)
        max-dissonance (apply max max-vals)]
    (fn [dissonance]
      (-> dissonance (- min-dissonance) (/ (- max-dissonance min-dissonance))))))

(defn sort-chords [chords]
  (let [grouped (group-by :name chords)]
    (into (vec (flatten (vals (dissoc grouped nil))))
          (grouped nil))))

(defn generate-chords
  [scales prev force]
  {:pre [(spec/assert (spec/* :scherz.scale/scale) scales)
         (spec/assert :scherz.chord/chord prev)
         (spec/assert ::force force)]}
  (let [scales (map keyword scales)
        {:keys [color dissonance gravity arc incline temper]} force
        chords (mapcat (partial chord-sets prev color arc incline temper) scales)
        cof-extent (u/extent (map b/pitch->brightness (:pitches prev)))
        normalize-dissonance (normalize-dissonance scales)
        score-color (fn [chord]
                      (-> (b/color (:cof-extent chord) cof-extent)
                          (/ 5)
                          (u/abs-diff color)))
        score-dissonance (fn [chord]
                           (-> (:dissonance chord)
                               normalize-dissonance
                               (u/abs-diff dissonance)))
        score-gravity (fn [chord]
                        (when-let [g (g/chord-gravity (:notes prev)
                                                      (:notes chord))]
                          (max (- gravity g) 0)))]
    (->> (apply-scores chords score-color score-dissonance score-gravity)
         sort-chords
         (map #(dissoc % :temper :cof-extent :type)))))

(defn initial-chords
  "Finds the first chord of a certain type within the given scales."
  ([scales tonic] (initial-chords scales tonic 0))
  ([scales tonic dissonance]
   {:pre [(spec/assert ::dissonance dissonance)
          (spec/assert (spec/* :scherz.scale/scale) scales)
          (spec/assert :scherz.brightness/tonic tonic)]}
   (let [scales (map keyword scales)
         chords (mapcat (partial c/chord-set tonic) scales)
         normalize-dissonance (normalize-dissonance scales)
         score-dissonance (fn [chord]
                            (-> (:dissonance chord)
                                normalize-dissonance
                                (u/abs-diff dissonance)))]
     (->> (map score-dissonance chords)
          (map vector chords)
          (u/min-by-coll second)
          (map first)
          sort-chords
          (map #(dissoc % :temper :cof-extent :type))))))

(defn- next-chord
  "Finds the next chord of a progression within the given scales.
   Seed is used if there is a tie between possible chord choices."
  ([scales prev force] (next-chord 0 scales prev force))
  ([seed scales prev force]
   (let [chords (generate-chords scales prev force)]
     (get (vec chords)
          (mod seed (count chords))))))

(defn generate-progression
  "Repeatedly calls next-chord to generate a chord progression."
  ([scales forces] (generate-progression scales forces nil))
  ([scales forces options]
   {:pre [(spec/assert (spec/* :scherz.scale/scale) scales)
          (spec/assert (spec/* ::force) forces)
          (spec/valid? (spec/nilable ::options) options)]}
   (let [{:keys [tonic seed dissonance]
          :or {tonic "C" seed 0 dissonance 0}} options]
     (reductions (partial next-chord (int seed) scales)
                 (nth (initial-chords scales tonic dissonance) seed)
                 forces))))
