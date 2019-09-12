(ns scherz.dissonance
  (:require [scherz.util :refer [floor avg]]
            [scherz.chord :refer [chord-set]]
            [scherz.scale :refer [scales]]))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn- gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

; using objects with numerator / denominator since cljs doesn't support ratios
(def freq-ratios
  (let [base-ratios [[1 1] [25 24] [9 8] [6 5] [5 4] [4 3] [45 32]
                     [3 2] [8 5] [5 3] [9 5] [15 8] [2 1]]]
    (vec (map (fn [[n d]] {:numerator n :denominator d})
              base-ratios))))

(defn- chord->ratios
  "Converts a set of notes into frequency ratios above the lowest note.
  (chord->ratios '(0 4 7)) -> [5/4 3/2]"
  [notes]
  (map (fn [note]
         (->> (first notes) (- note) freq-ratios))
       notes))

(defn- lcm [coll]
  (reduce (fn [a b]
            (/ (* a b) (gcd a b)))
          coll))

(defn- lcm-of-ratios
  "Finds a least common multiple from a set of ratios.
  Major triads have the ratios [5/4 3/2] which is equivalent to [5/4 6/4].
  This is seen as a 4:5:6 which has a least common multiple of 60."
  [ratios]
  (let [multiple (lcm (map :denominator ratios))
        normalize-ratio (fn [{:keys [denominator numerator]}]
                          (->> denominator (/ multiple) (* numerator)))]
    (lcm (map normalize-ratio ratios))))

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
  (->> (chord->ratios notes)
       lcm-of-ratios
       prime-factors
       frequencies
       (map (fn [[prime exponent]] (* exponent (dec prime))))
       (reduce +)
       inc))

(def scale-dissonance
  (->> scales
       (map (partial chord-set "C"))
       (map (fn [chords]
              (->> chords
                   (map :notes)
                   (map chord-dissonance)
                   avg)))
       (map vector scales)
       (into {})))

(defn normalize-dissonance
  "With a set of scales, returns a function that takes in a dissonance value and
   outputs a normalized dissonance value from 0 to 1."
  [scales]
  (let [dissonance-vals (->> scales
                             (mapcat (partial chord-set "C"))
                             (map :notes)
                             (map chord-dissonance))
        min-dissonance (apply min dissonance-vals)
        max-dissonance (apply max dissonance-vals)
        diff (- max-dissonance min-dissonance)]
    (fn [dissonance] (-> dissonance (- min-dissonance) (/ diff)))))

(chord-dissonance '(0 3 6 9))
