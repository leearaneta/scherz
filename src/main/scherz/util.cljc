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

(defn rotate [coll offset]
  (->> (cycle coll)
       (drop offset)
       (take (count coll))))

(defn floor [n]
  (Math/floor n))

(def chord-shapes
  {6 [[0 2 4 6] [0 2 4 5]]
   7 [[0 2 4 7] [0 2 4 6] [0 1 4 6] [0 3 4 6] [0 1 2 4]]
   8 [[0 2 4 6] [0 2 4 7]]})

(def scale-intervals
  (let [ionian-sequence  [2 2 1 2 2 2 1]]
    {:major              (rotate ionian-sequence 0)
     :dorian             (rotate ionian-sequence 1)
     :phrygian           (rotate ionian-sequence 2)
     :lydian             (rotate ionian-sequence 3)
     :mixolydian         (rotate ionian-sequence 4)
     :minor              (rotate ionian-sequence 5)
     :locrian            (rotate ionian-sequence 6)
     :harmonic-minor     [2 1 2 2 1 3 1]
     :melodic-minor-asc  [2 1 2 2 2 2 1]
     :super-locrian      [1 2 1 2 2 2 2]
     :neapolitan-major   [1 2 2 2 2 2 1]
     :locrian-major      [2 2 1 1 2 2 2]
     :neapolitan-minor   [1 2 2 2 1 3 1]
     :harmonic-major     [2 2 1 2 1 3 1]
     :melodic-minor-desc [2 1 2 2 1 2 2]
     :melodic-minor      [2 1 2 2 2 2 1]
     :melodic-major      [2 2 1 2 1 2 2]
     :lydian-minor       [2 2 2 1 1 2 2]
     :diminished         [1 2 1 2 1 2 1 2]
     :diminished2        [2 1 2 1 2 1 2 1]
     :augmented          [2 2 2 2 2 2]}))

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

(defn min-by [f coll]
  (loop [elem nil
         min infinity
         coll coll]
    (cond (empty? coll) elem
          :else (let [curr (first coll)
                      compare (f curr)]
                  (if (< compare min)
                    (recur curr compare (rest coll))
                    (recur elem min (rest coll)))))))

(defn max-by [f coll]
  (min-by (comp (fn [v] (/ 1 v)) f)
          coll))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f k v)])))

(defn- invert-asc [notes]
  (sort (cons (+ (first notes) 12)
              (next notes))))

(defn- invert-desc [notes]
  (sort (cons (- (last notes) 12)
              (next (reverse notes)))))

(defn invert
  [notes shift]
  (cond
    (zero? shift) notes
    (pos? shift) (recur (invert-asc notes) (dec shift))
    (neg? shift) (recur (invert-desc notes) (inc shift))))

