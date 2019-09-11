(ns scherz.util)

(defmacro fwhen [args body]
  (let [a (if (some #(= '& %) args)
               `(concat ~(-> args vec pop pop) ~(last args))
               args)]
    `(fn ~args
       (if (some nil? ~a) nil ~body))))

(defn avg [coll]
  (if (empty? coll)
    nil
    (/ (reduce + coll)
       (count coll))))

(defn abs [v]
  (max v (- v)))

(def infinity
  #?(:clj Integer/MAX_VALUE
     :cljs js/Infinity))

(def abs-diff (comp abs -))

(defn floor [n]
  (Math/floor n))

(def chord-shapes
  {6 [[0 2 4 6] [0 2 4 5]]
   7 [[0 2 4 7] [0 2 4 6] [0 1 4 6] [0 3 4 6] [0 1 2 4]]
   8 [[0 2 4 6] [0 2 4 7] [0 2 4 8] [0 2 5 7] [0 2 5 8]]})

(def scale-intervals
  (let [rotate (fn [coll offset]
                 (->> (cycle coll)
                      (drop offset)
                      (take (count coll))))
        ionian-sequence   [2 2 1 2 2 2 1]]
    {:major               (rotate ionian-sequence 0)
     :dorian              (rotate ionian-sequence 1)
     :phrygian            (rotate ionian-sequence 2)
     :lydian              (rotate ionian-sequence 3)
     :mixolydian          (rotate ionian-sequence 4)
     :minor               (rotate ionian-sequence 5)
     :locrian             (rotate ionian-sequence 6)
     :harmonic-minor      [2 1 2 2 1 3 1]
     :melodic-minor-asc   [2 1 2 2 2 2 1]
     :super-locrian       [1 2 1 2 2 2 2]
     :neapolitan-major    [1 2 2 2 2 2 1]
     :locrian-major       [2 2 1 1 2 2 2]
     :neapolitan-minor    [1 2 2 2 1 3 1]
     :harmonic-major      [2 2 1 2 1 3 1]
     :melodic-minor-desc  [2 1 2 2 1 2 2]
     :melodic-minor       [2 1 2 2 2 2 1]
     :melodic-major       [2 2 1 2 1 2 2]
     :lydian-minor        [2 2 2 1 1 2 2]
     :dominant-diminished [1 2 1 2 1 2 1 2]
     :diminished          [2 1 2 1 2 1 2 1]
     :augmented           [2 2 2 2 2 2]}))

(defn pitch->midi [pitch]
  (let [notes {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11}
        multiplier (if (= \# (last pitch)) 1 -1)]
    (-> (dec (count pitch))
        (* multiplier)
        (+ (notes (first pitch)))
        (mod 12))))

(defn base-chord [tonic scale chord-shape degree]
  (->> (cycle (scale-intervals scale))
       (reductions + (pitch->midi tonic))
       (drop (dec degree))
       (take (inc (last chord-shape)))
       (#(map (vec %) chord-shape))))

(def scales (keys scale-intervals))

(defn valid-scale? [scale]
  (some #(= % scale) scales))

(def chord-types
  {:M      [0 4 7 12]
   :m      [0 3 7 12]
   :°      [0 3 6 12]
   :+      [0 4 8 12]
   :M7     [0 4 7 11]
   :D7     [0 4 7 10]
   :m7     [0 3 7 10]
   :°7     [0 3 6 9]
   :ø7     [0 3 6 10]
   :+7     [0 4 8 10]
   :mM7    [0 3 7 11]
   :°M7    [0 3 6 11]
   :7sus2  [0 2 7 10]
   :7sus4  [0 5 7 10]
   :M7sus2 [0 2 7 11]
   :M7sus4 [0 5 7 11]
   :Madd2  [0 2 4 7]
   :madd2  [0 2 3 7]})

(defn chord-type [notes]
  (first (filter (fn [k] (= (k chord-types)
                            (map #(- % (first notes)) notes)))
                 (keys chord-types))))

(defn possible-types
  "Outputs all possible chord types given a set of scales."
  [scales]
  {:pre [(every? valid-scale? scales)]}
  (->> scales
       (map keyword)
       (mapcat (fn [scale]
                 (let [note-ct (count (scale-intervals scale))]
                   (for [shape (chord-shapes note-ct)
                         degree (range 1 (inc note-ct))]
                     (chord-type (base-chord "C" scale shape degree))))))
       distinct
       (remove nil?)))

(defn possible-type? [scales type]
  (some #(= % (keyword type)) (possible-types scales)))

(defn min-by
  ([f coll] (min-by 0 f coll))
  ([seed f coll]
   (loop [elems []
          min infinity
          coll coll]
     (if (empty? coll)
       (do
         (when (< 1 (count elems)) (print (str (count elems) " possibilities\n")))
         (get elems (mod seed (count elems)) nil))
       (let [curr (first coll)
             compare (f curr)]
         (cond (< compare min)
               (recur [curr] compare (rest coll))
               (= compare min)
               (recur (conj elems curr) min (rest coll))
               :else
               (recur elems min (rest coll))))))))

(defn max-by [f coll]
  (let [inverse (fn [v] (/ 1 v))]
   (min-by (comp inverse f) coll)))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f k v)])))

(defn condense
  ([notes] (condense notes 12))
  ([notes n]
   (distinct (map #(mod % n) notes))))

(defn- invert
  [flast coll shift]
  (if (zero? shift)
    coll
    (recur flast
           (->> (flast coll)
                (conj (vec (next coll)))
                (apply list))
           (dec shift))))

(def note-invert
  (let [f (fn [notes]
            (if (= (count (condense notes)) 3)
              (+ (second notes) 12)
              (+ (first notes) 12)))]
    (partial invert f)))

(def pitch-invert
  (let [f (fn [pitches]
            (if (= (count (distinct pitches)) 3)
              (second pitches)
              (first pitches)))]
    (partial invert f)))
