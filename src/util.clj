(ns scherz.util)

(defmacro fwhen [args body]
  (let [new-body `(if (some nil? ~args) nil ~body)]
    `(fn ~args ~new-body)))

(defn avg [coll]
  (if (empty? coll)
      nil
      (/ (reduce + coll)
         (count coll))))

(defn abs [v]
  (max v (- v)))

(def abs-diff (comp abs -))

(defn rotate [coll offset]
  (->> (cycle coll)
       (drop offset)
       (take (count coll))))

(def chord-shapes
  {6 [[0 2 4 7]]
   7 [[0 2 4 7] [0 2 4 6] [0 1 4 6] [0 3 4 6]]
   8 [[0 2 4 6] [0 2 4 7]]})

(def scale-intervals
  (let [ionian-sequence  [2 2 1 2 2 2 1]]
    {:diatonic           ionian-sequence
     :ionian             (rotate ionian-sequence 0)
     :major              (rotate ionian-sequence 0)
     :dorian             (rotate ionian-sequence 1)
     :phrygian           (rotate ionian-sequence 2)
     :lydian             (rotate ionian-sequence 3)
     :mixolydian         (rotate ionian-sequence 4)
     :aeolian            (rotate ionian-sequence 5)
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

(def chords
  {:M7     #{0 4 7 11}
   :D7     #{0 4 7 10}
   :m7     #{0 3 7 10}
   :d7     #{0 3 6 9}
   :mM7    #{0 3 7 11}
   :dM7    #{0 3 6 11}
   :7sus2  #{0 2 7 10}
   :7sus4  #{0 5 7 10}
   :m7-5   #{0 3 6 10}})

(defn pitch->midi [pitch]
  (let [pitch (name pitch)
        notes {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11}
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
         min Integer/MAX_VALUE
         coll coll]
    (cond (empty? coll) elem
          :else (let [curr (first coll)
                      compare (f curr)]
                  (if (< compare min)
                    (recur curr compare (rest coll))
                    (recur elem min (rest coll)))))))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))
