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

(spec/def ::color ::force-val)
(spec/def ::dissonance ::force-val)
(spec/def ::gravity ::force-val)

(spec/def ::force (spec/keys :req-un [::color ::dissonance ::gravity]))

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

(defn- chord-sets
  "Returns chords within the given scale and target color from the previous chord."
  [prev-chord target-color scale]
  (let [target-color (u/round (* 5 target-color))
        arcfs (if (= target-color 0) [+] [+ -])
        apply-arc (fn [arcf]
                    (-> (u/round (u/avg (:cof-extent prev-chord)))
                        (arcf target-color) ; add or subtract target color
                        ; isolate amount of brightness we need from new tonic
                        (- (b/scale-brightness scale))
                        b/brightness->pitch ; get new tonic
                        (c/chord-set scale)))]
    (mapcat apply-arc arcfs)))

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
        {:keys [color dissonance gravity]} force
        chords (mapcat (partial chord-sets prev color) scales)
        normalize-dissonance (normalize-dissonance scales)
        score-color (fn [chord]
                      (-> (b/color (:cof-extent chord) (:cof-extent prev))
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
         (u/distinct-by (comp g/sink-octave :notes))
         sort-chords)))

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
          sort-chords))))

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
          :or {tonic "C" seed 0 dissonance 0}} options
         initial-chords (initial-chords scales tonic dissonance)]
     (reductions (partial next-chord seed scales)
                 (nth initial-chords (mod seed (count initial-chords)))
                 forces))))
