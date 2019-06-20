(ns scherz.chord
  (:use [overtone.live]))

(defn compress-chord [notes]
  (distinct (map (fn [x] (mod x 12)) notes)))

(defn base-chord
  ([degree tonic mode] (base-chord degree tonic mode 3))
  ([degree tonic mode note-ct]
   (compress-chord (chord-degree degree
                                 (-> tonic name (str 1) keyword)
                                 mode
                                 note-ct))))

(compress-chord (chord-degree :i :C0 :major))

(defn rotate-chord
  ([chord] (rotate chord 1))
  ([chord offset] (->> (cycle chord)
                       (drop offset)
                       (take (count chord)))))

(defn chord-rotations [chord]
  (map (partial rotate-chord chord)
       (range 0 (count chord))))

(defn chord-distance [current-chord target-chord]
  (mapv (fn [note1 note2]
          (-> note1
              (- note2)
              (#(Math/abs %))
              (#(min % (- 12 %)))))
        current-chord
        target-chord))

(def chord1 (base-chord :i :C :major))
(def chord2 (base-chord :v :C :melodic-minor))
(def chord3 (nth (chord-rotations chord2) 1))
(chord-distance chord1 chord3)

(defn min-by [f coll]
  (:elem (reduce (fn [acc val]
                   (let [compare (f val)]
                     (if (< compare (:min acc))
                       {:elem val :min compare}
                       acc)))
                 {:elem (first coll) :min (f (first coll))}
                 (rest coll))))

(defn closest-voicing [current-chord target-chord]
  (let [compressed (compress-chord current-chord)
        order (map #(.indexOf compressed %) (sort compressed))
        ; orient the target chord in the same order as the current chord
        sorted-target (-> target-chord compress-chord sort vec)
        reordered-target (map #(get sorted-target %) order)
        rotations (chord-rotations reordered-target)
        ; find a rotation of the target chord that yields minimum distance
        chord-distances (map (partial chord-distance compressed) rotations)
        zipped (map vector rotations chord-distances)
        optimal-rotation (first (min-by #(sum (second %)) zipped))
        diffs (mapv - optimal-rotation compressed)]
    (mapv + current-chord diffs)))

(closest-voicing chord1 chord2)

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
  (chord-set :C [:lydian :mixolydian :melodic-minor :phrygian] 3))

(defn normalized-chord-distances [current-chord target-chords]
  ; this is like O(n^3) but n is pretty small
  (let [rotations (chord-rotations current-chord)
        min-distance (fn [target-chord]
                       (apply min (map #(sum (chord-distance target-chord %))
                                       rotations)))]
    (map min-distance target-chords)))

(normalized-chord-distances chord1 chords)

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

freq-ratios

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

(chord->ratios chord1)

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

(ratios->lcm (chord->ratios chord1))

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
