(ns scherz.dissonance
  (:require [scherz.util :refer [map-vals chord-shapes base-chord
                                 scale-intervals note-invert floor avg]]))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn- gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn multiply-ratio
  [{numerator :numerator :as ratio} scalar]
  (into ratio
        {:numerator (* numerator scalar)}))

(def freq-ratios
  (let [base-ratios [[1 1] [25 24] [9 8] [6 5] [5 4] [4 3]
                     [45 32] [3 2] [8 5] [5 3] [9 5] [15 8]]
        add-octaves (fn [[index ratio]]
                      (->> (/ index 12) floor (exp 2) (multiply-ratio ratio)))]
    (->> (cycle base-ratios)
         (take (* (count base-ratios) 8))
         (map (fn [[n d]] {:numerator n :denominator d}))
         (map vector (range))
         (map add-octaves)
         vec)))

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
  (map-vals (fn [scale intervals]
              (let [note-ct (count intervals)]
                (avg (for [degree (range 1 (inc note-ct))
                           shape (chord-shapes note-ct)
                           inversion (range (count shape))]
                       (-> (base-chord "C" scale shape degree)
                           (note-invert inversion)
                           chord-dissonance)))))
            scale-intervals))
