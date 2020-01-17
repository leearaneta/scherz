(ns scherz.dissonance
  (:require [scherz.chord :refer [base-chord-sets]]))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn- gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))


; using objects with numerator / denominator since cljs doesn't support ratios
(def freq-ratios
  (let [floor (fn [n] (Math/floor n))
        multiply-ratio (fn [{numerator :numerator :as ratio} scalar]
                         (into ratio
                               {:numerator (* numerator scalar)}))
        base-ratios [[1 1] [25 24] [9 8] [6 5] [5 4] [4 3]
                     [45 32] [3 2] [8 5] [5 3] [9 5] [15 8]]
        add-octaves (fn [[index ratio]]
                      (->> (/ index 12) floor (exp 2) (multiply-ratio ratio)))]
    (->> (cycle base-ratios)
         (take (* (count base-ratios) 8))
         (map (fn [[n d]] {:numerator n :denominator d}))
         (map-indexed vector)
         (map add-octaves)
         vec)))

(defn- chord-ratios
  "Converts a set of notes into frequency ratios above the lowest note.
  (chord->ratios '(0 4 7)) -> [1/1 5/4 3/2]"
  [notes]
  (map (fn [note]
         (freq-ratios (- note (first notes))))
       notes))

(defn- lcm [coll]
  (reduce (fn [a b]
            (/ (* a b) (gcd a b)))
          coll))

(defn- lcm-of-ratios
  "Finds a least common multiple from a set of ratios.
  Major triads have the ratios [1/1 5/4 3/2] which is equivalent to [4/4 5/4 6/4].
  This is seen as a 4:5:6 which has a least common multiple of 60."
  [ratios]
  (let [multiple (lcm (map :denominator ratios))]
    (->> ratios (map :numerator) (map (partial * multiple)) lcm)))

(defn prime-factors
  ([n] (prime-factors 2 n))
  ([f n]
   (cond
     (> f (Math/sqrt n)) (if (= n 1) [] [n])
     (zero? (mod n f)) (cons f (prime-factors f (/ n f)))
     :else (recur (inc f) n))))

(defn chord-dissonance
  "Measures dissonance of a chord based on Euler's Gradus Suavitatis."
  [notes]
  (->> notes
       chord-ratios
       lcm-of-ratios
       prime-factors
       frequencies
       (map (fn [[prime exponent]] (* exponent (dec prime))))
       (reduce +)
       inc))

(defn normalize-dissonance
  "With a set of scales, returns a function that takes in a dissonance value and
   outputs a normalized dissonance value from 0 to 1."
  [scales]
  (let [dissonance-vals (->> scales
                             (mapcat (partial base-chord-sets))
                             (map :notes)
                             (map chord-dissonance))
        min-dissonance (apply min dissonance-vals)
        max-dissonance (apply max dissonance-vals)
        diff (- max-dissonance min-dissonance)]
    (fn [dissonance] (-> dissonance (- min-dissonance) (/ diff)))))

