(ns scherz.dissonance)

(defn exp [x n]
  (reduce * (repeat n x)))

(defn- gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn- lcm [a b]
  (/ (* a b) (gcd a b)))

(defn- gcd-ratios [ratios]
  (/ (reduce gcd (map :numerator ratios))
     (reduce lcm (map :denominator ratios))))

; using objects with numerator / denominator since cljs doesn't support ratios
(def freq-ratios
  (let [floor (fn [n] (Math/floor n))
        multiply-ratio (fn [{numerator :numerator :as ratio} scalar]
                         (into ratio
                               {:numerator (* numerator scalar)}))
        base-ratios [[1 1] [25 24] [9 8] [6 5] [5 4] [4 3]
                     [45 32] [3 2] [8 5] [5 3] [9 5] [15 8]]
        add-octave (fn [[index ratio]]
                      (->> (/ index 12) floor (exp 2) (multiply-ratio ratio)))]
    (vec (->> (cycle base-ratios)
              (take (* (count base-ratios) 8))
              (map (fn [[n d]] {:numerator n :denominator d}))
              (map-indexed vector)
              (map add-octave)))))

(defn notes->ratios
  "Converts a set of notes into frequency ratios above the lowest note.
  (notes->ratios '(0 4 7)) -> [1/1 5/4 3/2]"
  [notes]
  (map (fn [note]
         (freq-ratios (- note (first notes))))
       notes))

(defn ratios->terms [ratios]
  (let [g (gcd-ratios ratios)
        div #?(:clj quot :cljs (fn [n d] (Math/floor (/ n d))))
        simplify (fn [ratio]
                   (-> (:numerator ratio) (div g) (/ (:denominator ratio)) int))]
    (map simplify ratios)))

(defn prime-factors
  ([n] (prime-factors 2 n))
  ([f n]
   (cond
     (> f (Math/sqrt n)) (if (= n 1) [] [n])
     (zero? (mod n f)) (cons f (prime-factors f (/ n f)))
     :else (recur (inc f) n))))

(defn dissonance
  "Measures dissonance of a set of midi notes based on Euler's Gradus Suavitatis."
  [notes]
  (->> notes
       notes->ratios
       ratios->terms
       (reduce lcm)
       prime-factors
       frequencies
       (map (fn [[prime exponent]] (* exponent (dec prime))))
       (reduce +)))
