(ns scherz.consonance
  (:require [scherz.util]))

(refer 'scherz.util)

(def freq-ratios
  (let [base-ratios [25/24 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8 2/1]
        add-octaves (fn [i v]
                      (->> (/ i 12)
                           (#(Math/floor %))
                           (#(Math/pow 2 %))
                           int
                           (* v)))]
    (->> (cycle base-ratios)
         (take (* (count base-ratios) 8))
         (map-indexed add-octaves)
         vec)))

(defn- chord->ratios
  "Converts a set of notes into frequency ratios above the lowest note.
  (chord->ratios '(0 4 7)) -> [5/4 3/2]"
  [notes]
  (map (fn [note]
         (->> (first notes)
              (- note)
              dec
              freq-ratios))
       (rest notes)))

(defn- gcd [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))
 
(defn- lcm [a b]
  (/ (* a b) (gcd a b)))

(defn- lcmv [& v] (reduce lcm v))

(defn- lcm-of-ratios
  "Finds a least common multiple from a set of ratios.
  Major triads have the ratios [5/4 3/2] which is equivalent to [5/4 6/4].
  This is seen as a 4:5:6 which has a least common multiple of 60."
  [ratios]
  (let [denominator #(if (ratio? %) (denominator %) 1)
        numerator #(if (ratio? %) (numerator %) %)
        multiple (apply lcmv (map denominator ratios))
        normalize-ratio (fn [ratio]
                          (->> (denominator ratio)
                               (/ multiple)
                               (* (numerator ratio))))]
    (->> ratios
         (map normalize-ratio)
         (cons multiple)
         (apply lcmv)
         int)))

(defn factors-starting-at [f n]
  (cond
    (> f (Math/sqrt n)) (if (= n 1) [] [n])
    (zero? (mod n f)) (cons f (factors-starting-at f (/ n f)))
    :else (recur (inc f) n)))

(defn prime-factors-of [n]
  (factors-starting-at 2 n))

(defn- consonance
  "Measures consonance of a chord based on Euler's Gradus Suavitatis."
  [notes]
  (->> (chord->ratios notes)
       lcm-of-ratios
       prime-factors-of
       frequencies
       (map (fn [[prime exponent]] (* exponent (dec prime))))
       (reduce +) inc))

(def scale-consonance
  "Maps each scale to a list of consonance values, each one representing the
  consonance of one chord.  Can be used to ballpark how 'consonant' a scale is."
  (map-vals (fn [intervals]
              (let [note-ct (count intervals)]
                (for [shape (chord-shapes note-ct)
                      degree (range 1 (inc note-ct))]
                  (->> (cycle intervals)
                       (reductions + 0)
                       (drop (dec degree))
                       (take (inc (last shape)))
                       (#(map (vec %) shape))
                       consonance))))
            scale-intervals))
