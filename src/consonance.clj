(ns scherz.consonance
  (:use [overtone.live]))

(def degree
  (zipmap (vals DEGREE) (keys DEGREE)))

(defn chord-set
  ([tonic modes] (chord-set tonic modes 3))
  ([tonic modes note-ct]
   (mapcat (fn [mode]
             (map #(base-chord (degree %) tonic mode note-ct)
                  (range 1 8)))
           modes)))

(def chords
  (chord-set :C [:melodic-minor :lydian-minor] 4))

(defn chord-distances [source-chord target-chords]
  (map (partial chord-distance source-chord) target-chords))

(chord-distances chord1 chords)

(def freq-ratios
  (let [base-ratios [25/24 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8 2/1]
        add-octaves (fn [i v]
                      (->> (/ i 12)
                           (#(Math/floor %))
                           (#(Math/pow 2 %))
                           int (* v)))]
    (->> (cycle base-ratios)
         (take (* (count base-ratios) 8))
         (map-indexed add-octaves)
         vec)))

(defn chord->ratios [chord]
  (map (fn [note]
         (->> (first chord)
              (- note)
              dec
              freq-ratios))
       (rest chord)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))
 
(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v] (reduce lcm v))

(defn ratios->lcm [ratios]
  (let [denominator #(if (ratio? %) (denominator %) 1)
        numerator #(if (ratio? %) (numerator %) %)
        multiple (apply lcm (map denominator ratios))
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

(defn euler-gradus [chord]
  (->> chord
       chord->ratios
       ratios->lcm
       prime-factors-of
       frequencies vec
       (map (fn [[prime exponent]] (* exponent (dec prime))))
       sum inc))

(euler-gradus chord1)

(defn key-tension [tonic chord]
  (let [tensions [0 5 3 1 1 3 5 0 4 2 2 5]]
    (->> chord
         (map #(- % (NOTES tonic)))
         compress-chord
         (map tensions)
         sum)))

(map (partial key-tension :C) chords)

