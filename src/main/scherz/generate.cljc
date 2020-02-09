(ns scherz.generate
  (:require [clojure.core.reducers :as r]
            [clojure.spec.alpha :as spec]
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

(spec/def ::tension (spec/keys :req-un [::color ::dissonance ::gravity]
                               :opt-un [::arc ::incline]))

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

(defn- chord-sets
  "Returns chords within the given scale and target color from the previous chord."
  [prev-chord target-color curve incline scale]
  (let [target-color (Math/round (double (* 5 target-color)))
        fcurves (cond
                  (or (= target-color 0) (= (keyword curve) :asc)) [+]
                  (= (keyword curve) :desc) [-]
                  :else [+ -])
        apply-curve (fn [fcurve]
                      (-> (b/scale-brightness (:scale prev-chord))
                             (+ (b/pitch->brightness (:tonic prev-chord)))
                             (fcurve target-color) ; add or subtract target color
                             ; isolate amount of brightness we need from new tonic
                             (- (b/scale-brightness scale))
                             b/brightness->pitch ; get new tonic
                             (c/chord-set scale)))
        chords (mapcat apply-curve fcurves)]
    (if (some? incline)
      (let [fdirection (if (= (keyword incline) :asc) neg? pos?)
            centroid (u/avg (:notes prev-chord))
            right-direction? (comp fdirection (partial - centroid) u/avg :notes)]
        (filter right-direction? chords))
      chords)))

(def scale-dissonance
  (->> s/scales
       (map (comp u/extent (partial map :dissonance) c/base-chord-sets))
       (map vector s/scales)
       (into {})))

(defn normalize-dissonance
  "With a set of scales, returns a function that takes in a dissonance value and
   outputs a normalized dissonance value from 0 to 1."
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
        prev #?(:clj prev :cljs (js->clj prev :keywordize-keys :true))
        tension #?(:clj tension :cljs (js->clj tension :keywordize-keys :true))
        {:keys [color dissonance gravity curve direction]} tension
        chords (mapcat (partial chord-sets prev color curve direction) scales)
        extent (u/extent (map b/pitch->brightness (:pitches prev)))
        score-color (fn [chord]
                      (-> (b/chord-color (:extent chord) extent)
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
   (let [chords (time (generate-chords scales prev tension))]
     (get (vec chords)
          (mod seed (count chords))))))

(defn generate-progression
  "Repeatedly calls next-chord to generate a chord progression."
  ([scales tensions] (generate-progression scales tensions nil))
  ([scales tensions options]
   {:pre [(spec/assert (spec/* :scherz.scale/scale) scales)
          (spec/assert (spec/* ::tension) tensions)
          (spec/assert (spec/nilable ::options) options)]}
   (let [options #?(:clj options :cljs (js->clj options :keywordize-keys :true))
         {:keys [tonic type seed]
          :or {tonic "C"
               type (name (first (c/possible-chord-types scales)))
               seed 0}} options]
     (reductions (partial next-chord (int seed) scales)
                 (initial-chord scales tonic type)
                 tensions))))
