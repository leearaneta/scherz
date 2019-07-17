(ns scherz.util)

(defmacro fwhen [args body]
  (let [new-body `(if (some nil? ~args) nil ~body)]
    `(fn ~args ~new-body)))

(defn avg [coll]
  (/ (reduce + coll)
     (count coll)))

(defn abs [v]
  (max v (- v)))

(def abs-diff (comp abs -))

(def modes
  (let [ionian-sequence     [2 2 1 2 2 2 1]
        rotate (fn [scale-sequence offset]
                 (take (count scale-sequence)
                       (drop offset (cycle scale-sequence))))]
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
     :lydian-minor       [2 2 2 1 1 2 2]}))

(defn pitch->midi [pitch]
  (let [pitch (name pitch)
        notes {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11}
        multiplier (if (= \# (last pitch)) 1 -1)]
    (-> (dec (count pitch))
        (* multiplier)
        (+ (notes (first pitch)))
        (mod 12))))

(defn base-chord [tonic mode note-ct degree]
  (->> (cycle (modes mode))
       (reductions + (pitch->midi tonic))
       (drop (dec degree))
       (take-nth 2)
       (take note-ct)))

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
