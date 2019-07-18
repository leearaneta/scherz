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

(defn- chord->ratios [notes]
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

(defn- lcm-of-ratios [ratios]
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

; euler's gradus suavitatis
(defn chord-consonance [notes]
  (->> (chord->ratios notes)
       lcm-of-ratios
       prime-factors-of
       frequencies
       (map (fn [[prime exponent]] (* exponent (dec prime))))
       (reduce +) inc))

