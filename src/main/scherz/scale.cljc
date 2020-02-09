(ns scherz.scale
  (:require [clojure.spec.alpha :as spec]))

(def scale-intervals
  (let [rotate (fn [coll offset]
                 (->> (cycle coll)
                      (drop offset)
                      (take (count coll))))
        ionian-sequence   [2 2 1 2 2 2 1]]
    {:major               (rotate ionian-sequence 0)
     :dorian              (rotate ionian-sequence 1)
     :phrygian            (rotate ionian-sequence 2)
     :lydian              (rotate ionian-sequence 3)
     :mixolydian          (rotate ionian-sequence 4)
     :minor               (rotate ionian-sequence 5)
     :locrian             (rotate ionian-sequence 6)
     :harmonic-minor      [2 1 2 2 1 3 1]
     :melodic-minor-asc   [2 1 2 2 2 2 1]
     :super-locrian       [1 2 1 2 2 2 2]
     :neapolitan-major    [1 2 2 2 2 2 1]
     :locrian-major       [2 2 1 1 2 2 2]
     :neapolitan-minor    [1 2 2 2 1 3 1]
     :harmonic-major      [2 2 1 2 1 3 1]
     :melodic-minor-desc  [2 1 2 2 1 2 2]
     :melodic-minor       [2 1 2 2 2 2 1]
     :melodic-major       [2 2 1 2 1 2 2]
     :lydian-minor        [2 2 2 1 1 2 2]
     :dominant-diminished [1 2 1 2 1 2 1 2]
     :diminished          [2 1 2 1 2 1 2 1]
     :augmented           [2 2 2 2 2 2]}))

(def scales (keys scale-intervals))

(defn valid-scale? [scale]
  (some (partial = (keyword scale)) scales))

(spec/def ::scale valid-scale?)

