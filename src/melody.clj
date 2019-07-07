(ns scherz.melody
  (:use [overtone.live]))

(defn interval-tension [current-note midi-scale]
  (let [nearest-position (count (take-while #(< % current-note)
                                            midi-scale))]
    (map-indexed
     (fn [i v] (->> i (- nearest-position) (#(Math/abs %))))
     midi-scale)))

(defn harmonic-tension [chord midi-scale]
  (let [lower #(mod % 12)
        in? (fn [coll elem]
              (some #(= elem %) coll))
        consonant? #(in? (map lower chord) (lower %))
        resolves-downwards? #(in? (map (comp lower inc) chord) (lower %))
        resolves-upwards? #(in? (map (comp lower dec) chord) (lower %))]
    (map (fn [note]
           (cond (consonant? note) 0
                 (resolves-downwards? note) 3
                 (resolves-upwards? note) 2
                 :else 1))
         midi-scale)))

(defn midi-scale
  ([root mode] (midi-scale root mode 12))
  ([root mode octaves]
   (let [base (NOTES root)
         intervals (SCALE mode)
         interval-cycle (take (* (count intervals) octaves)
                              (cycle intervals))]
     (drop-last (reductions + base interval-cycle)))))
