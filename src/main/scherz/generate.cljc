(ns scherz.generate
  (:require [clojure.core.reducers :as r]
            [scherz.util :as u]
            [scherz.scale :as s]
            [scherz.gravity :as g]
            [scherz.brightness :as b]
            [scherz.chord :as c]))

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
         (map vector chords) ; get tuples of [chord score]
         (u/min-by-coll cost) ; choose chord(s) with lowest scores
         (map first))))

(defn- chord-sets
  "Returns chords within the given scale and target color from the previous chord."
  [prev-chord target-color curve direction scale]
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
    (if (some? direction)
      (let [fdirection (if (= (keyword direction) :asc) neg? pos?)
            centroid (u/avg (:notes prev-chord))]
        (filter (comp fdirection (partial - centroid) u/avg :notes) chords))
      chords)))

(defn- valid-tension? [{:keys [color dissonance gravity]}]
  (every? #(<= 0 % 1) [color dissonance gravity]))

(def scale-dissonance
  (reduce (fn [acc scale]
            (let [dissonance-vals (map :dissonance (scale c/base-chord-sets))]
              (assoc acc scale [(apply min dissonance-vals)
                                (apply max dissonance-vals)])))
          {} s/scales))

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
  {:pre [(every? s/valid-scale? scales)
         (valid-tension? tension)]}
  (let [scales #?(:clj scales :cljs (map keyword scales))
        prev #?(:clj prev :cljs (js->clj prev :keywordize-keys :true))
        tension #?(:clj tension :cljs (js->clj tension :keywordize-keys :true))
        {:keys [color dissonance gravity curve direction]} tension
        chords (mapcat (partial chord-sets prev color curve direction) scales)
        brightness-extent (u/extent (map b/pitch->brightness (:pitches prev)))
        score-color (fn [chord]
                      (-> (apply (:fcolor chord) brightness-extent)
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
    (map (fn [chord] (dissoc chord :root :bass :type :fcolor))
         (apply-scores chords score-color score-dissonance score-gravity))))

(defn initial-chord
  "Finds the first chord of a certain type within the given scales."
  ([scales type] (initial-chord scales type "C"))
  ([scales type tonic]
   {:pre [(every? s/valid-scale? scales)
          (b/valid-pitch? tonic)
          (c/possible-chord-type? scales type)]}
   (dissoc (->> (map keyword scales)
                (mapcat (partial c/chord-set tonic))
                (u/find-coll (comp (partial = type) :type)))
           :root :bass :type :fcolor)))

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
  [scales tensions options]
  (let [options #?(:clj options :cljs (js->clj options :keywordize-keys :true))
        {:keys [root type seed]
         :or {root "C"
              type (name (first (c/possible-chord-types scales)))
              seed 0}} options]
    (reductions (partial next-chord (int seed) scales)
                (initial-chord scales type root)
                tensions)))
