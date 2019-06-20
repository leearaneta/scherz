(ns scherz.melody
  (:use [overtone.live]))

(defn interval-tension [current-note midi-scale]
  (let [nearest-position (count (take-while #(< % current-note) midi-scale))]
    (map-indexed
     (fn [i v] (->> i (- nearest-position) (#(Math/abs %))))
     midi-scale)))

(defn modal-tension [mode]
  (let [intervals (vec (SCALE mode))
        consonant? (fn [index]
                     (some #(= index %) [0 2 4 7]))
        resolves-downwards? (fn [index]
                              (and (consonant? (- index 1))
                                   (->> (- index 1) intervals (= 1))))
        resolves-upwards? (fn [index]
                            (and (consonant? (+ index 1))
                                 (->> index intervals (= 1))))]
    (map (fn [index]
           (cond (consonant? index) 1
                 (resolves-downwards? index) 4
                 (resolves-upwards? index) 3
                 :else 2))
         (range 0 7))))

(defn harmonic-tension [chord midi-scale]
  (let [lower #(mod % 12)
        in? (fn [coll elem]
              (some #(= elem %) coll))
        consonant? #(in? (map lower chord) (lower %))
        resolves-downwards? #(in? (map (comp lower inc) chord) (lower %))
        resolves-upwards? #(in? (map (comp lower dec) chord) (lower %))]
    (map (fn [note]
           (cond (consonant? note) 1
                 (resolves-downwards? note) 4
                 (resolves-upwards? note) 3
                 :else 2))
         midi-scale)))

(defn midi-scale
  ([root mode] (midi-scale root mode 12))
  ([root mode octaves]
   (let [base (NOTES root)
         intervals (SCALE mode)
         interval-cycle (take (* (count intervals) octaves) (cycle intervals))]
     (drop-last (reductions + base interval-cycle)))))
