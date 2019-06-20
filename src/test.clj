(ns test.overtone
  (:use [overtone.live]
        [overtone.inst.piano]
        [clojure.string]))

; use frequencies instead of pitches
; use multiples of BPM / beat
; split into motifs
; define measure of consonance (using harmonic series and root of chords)
; make this more declarative

; TENSION AS A RESOURCE??

; stuff that affects transition vector (somewhat ordered):
 ; tension already used (should be more likely to resolve with less tension left)
 ; distance between notes, this is probably always the same
 ; syncopation
 ; pitch relative to current chord
 ; pitch relative to tonic

; motifs are more likely to resolve at less tense pitches and on the down beat
; after resolution, we can start another motif or reuse a previous one

; separate randomness from other code since it isn't referentially transparent

; create a bunch of functions:
 ; previous note => tension vector
 ; current chord => tension vector

; if the next note is above prev-note, multiply by a factor

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

(defn pop-string [s]
  (subs s 0 (- (count s) 1)))

(defn valid-direction? [direction]
  (or (= direction :asc) (= direction :desc)))

(defn shift-pitch
  ([direction pitch] (shift-pitch direction pitch 1))
  ([direction pitch amt]
   {:pre [(valid-direction? direction)]}
   (if (== amt 0) (keyword pitch)
       (let [pitch (name pitch)
             to-remove (if (= direction :asc) "b" "#")
             to-add (if (= direction :asc) "#" "b")]
         (if (ends-with? pitch to-remove)
           (recur direction (pop-string pitch) (- amt 1))
           (recur direction (str pitch to-add) (- amt 1)))))))

(def sharpen (partial shift-pitch :asc))
(def flatten (partial shift-pitch :desc))

(defn fifths
  ([root] (fifths root :asc))
  ([root direction]
   {:pre [(valid-direction? direction)]}
   (let [base-fifths (if (= direction :asc)
                       ["F" "C" "G" "D" "A" "E" "B"]
                       ["B" "E" "A" "D" "G" "C" "F"])
         shift-fn (if (= direction :asc) sharpen flatten)
         natural-root (subs (name root) 0 1)
         initial-accidental (subs (name root) 1)
         shift (fn [index value]
                 (->> (/ index 7)
                      (#(Math/floor %))
                      (shift-fn value)))]
     (lazy-seq (->> base-fifths
                    (map #(keyword (str % initial-accidental)))
                    cycle
                    (map-indexed shift)
                    (drop (.indexOf base-fifths natural-root)))))))

(circle-of-fifths :A :major)

(defn scale-brightness [mode]
  (let [cumulative-intervals (reductions + (SCALE mode))
        interval-brightness [0 -5 2 -3 4 -1 0 1 -4 3 -2 5]]
   (reduce (fn [acc interval]
             (+ acc (get interval-brightness interval 0)))
           0 cumulative-intervals)))

(defn circle-of-fifths
  ([root] (circle-of-fifths root :major))
  ([root mode]
   (let [bright? (pos? (scale-brightness mode))
         upper-arc (take (if bright? 6 5)
                         (drop 1 (fifths root)))
         lower-arc (take (if bright? 6 7)
                         (fifths root :desc))]
     (into upper-arc lower-arc))))

(circle-of-fifths :C :locrian)

(defn pitch-scale [root mode]
  (let [circle (vec (circle-of-fifths root mode))
        root-index (.indexOf circle root)]
    (->> (SCALE mode)
         (reductions +)
         (map #(-> % (* 7) (+ root-index) (mod 12)))
         (mapv circle)
         pop (into [root]))))

; rhythmic tension
; notes played closer together have more tension
(defn valid-positions [current-position]
  (map #(if (> % current-position) % nil) (range 0 320)))

(defn distance-tension [current-position subdivisions-per-beat beats-per-measure]
  (map (fn [position]
         (when position
           (-> current-position
               (- position)
               (+ (* subdivisions-per-beat beats-per-measure))
               (/ subdivisions-per-beat))))
       (valid-positions current-position)))

; syncopated notes have more tension
(defn syncopation-tension [current-position subdivisions-per-beat]
  (map (fn [position]
         (when position
           (-> position
               (mod subdivisions-per-beat)
               (/ subdivisions-per-beat)
               (#(if (= % 0) 0 (denominator %))))))
       (valid-positions current-position)))

; let's make some chord progressions

(defn neo-riemann [[root shape]]
  (let [new-roots (->> (if (= shape :major) :asc :desc)
                       (fifths root)
                       (drop 3)
                       (take 2)
                       (cons root))
        new-shape (if (= shape :major) :minor :major)]
    (map vector new-roots (repeat 3 new-shape))))

(defn reptree [f val]
  {:value val
   :children (map (partial reptree f) (f val))})

(defn prune [n {:keys [value children]}]
  (if (= n 0) {:value value :children nil}
      {:value value
       :children (map (partial prune (dec n)) children)}))

(prune 3 (reptree neo-riemann [:A :major]))

(defn foldtree [f g a data]
  (cond (map? data)
        (let [{value :value children :children} data]
          (f value (foldtree f g a children)))
        (seq? data)
        (let [[child & rest] data]
          (g (foldtree f g a child) (foldtree f g a rest)))
        (nil? data) a))

(def depth (partial foldtree
                    (fn [_ x] (inc x))
                    (fn [x _] x)
                    0))

(defn maptree [f]
  (partial foldtree
           (fn [value children] {:value (f value) :children children})
           cons
           nil))

(depth (prune 5 (reptree neo-riemann [:A :major])))

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

(defn factors-starting-at [f n]
  (cond
    (> f (Math/sqrt n)) (if (= n 1) [] [n])
    (zero? (mod n f)) (cons f (factors-starting-at f (/ n f)))
    :else (recur (inc f) n)))

(defn prime-factors-of [n]
  (factors-starting-at 2 n))

(def freq-ratios
  (let [base-ratios [25/24 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8 2/1]
        add-octaves (fn [i v]
                      (->> (/ i 12)
                           (#(Math/floor %))
                           (#(Math/pow 2 %))
                           int (* v)))]
    (->> (cycle base-ratios)
         (take (* (count base-ratios) 8))
         (map-indexed add-octaves))))

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

