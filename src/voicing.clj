(ns scherz.voicing
  (:use [overtone.live]))

(defn base-chord
  ([degree tonic mode] (base-chord degree tonic mode 3))
  ([degree tonic mode note-ct]
   (chord-degree degree
                 (-> tonic name (str -1) keyword)
                 mode
                 note-ct)))

(defn compress [notes]
  (distinct (map (fn [x] (mod x 12)) notes)))

(def compressed-chord (comp compress base-chord))

(def chord1 (compressed-chord :i :C :major))
(def chord2 (compressed-chord :v :C :melodic-minor))

; voice leading stuff
(defn min-by [f coll]
  (:elem (reduce (fn [acc val]
                   (let [compare (f val)]
                     (if (< compare (:min acc))
                       {:elem val :min compare}
                       acc)))
                 {:elem (first coll) :min (f (first coll))}
                 (rest coll))))


(defn note-distance [current-note target-note]
  (let [diff (- target-note current-note)]
    (min-by #(Math/abs %)
            [diff (+ diff 12) (- diff 12)])))

(defn- vecs->map [vecs]
  (reduce (fn [acc [key val]]
            (let [acc-val (acc key)]
              (cond (nil? acc-val) (conj acc {key val})
                    (seq? acc-val) (conj acc {key (conj acc-val val)})
                    :else (conj acc {key [acc-val val]}))))
          {} vecs))

(defn- chord-transition [source-chord target-chord]
  (if (> (count target-chord) (count source-chord))
    (->> (chord-transition target-chord source-chord)
         (map (fn [[source distance]]
                [(mod (+ source distance) 12) (* -1 distance)])))
    (loop [transition {}
           sources-remaining source-chord
           targets-remaining target-chord]
      (cond (seq sources-remaining)
            (let [source (first sources-remaining)
                  cost (fn [[_ distance]] (Math/abs distance))
                  [target distance] (->> target-chord
                                         (map (partial note-distance source))
                                         (map vector target-chord)
                                         (min-by cost))]
              (recur (conj transition {source distance})
                     (next sources-remaining)
                     (remove (partial = target) targets-remaining)))
            (seq targets-remaining)
            (let [target (first targets-remaining)
                  cost (fn [[source distance]]
                         (-> (Math/abs distance) (* 2) (- (transition source))))
                  [source distance] (->> source-chord
                                         (map (partial note-distance target))
                                         (map vector source-chord)
                                         (min-by cost))]
              (recur (conj transition {source distance})
                     sources-remaining
                     (next targets-remaining)))
            :else (into [] transition)))))

(defn voice-chord [source-chord target-chord]
  (let [transition (vecs->map (chord-transition (compress source-chord)
                                                (compress target-chord)))
        voice-note (fn [note]
                    (let [compressed (mod note 12)
                          octaves (-> note (/ 12) (#(Math/floor %)) int)
                          distance (transition compressed)
                          apply-distance #(-> % (+ compressed) (+ (* octaves 12)))]
                      (if (vector? distance) ; if the note is going to multiple places
                        (map apply-distance distance)
                        (apply-distance distance))))]
    (flatten (map voice-note source-chord))))

(defn chord-distance [source-chord target-chord]
  (reduce (fn [total [_ distance]]
            (+ total (Math/abs distance)))
          0 (chord-transition source-chord target-chord)))

(def chord3 (compressed-chord :v :C :major 4))

(chord-transition chord1 chord3)
(voice-chord chord1 chord3)
(chord-distance chord1 chord3)

(def degree
  (zipmap (vals DEGREE) (keys DEGREE)))

(defn chord-set
  ; TODO: update this to handle multiple note counts
  ([tonic modes] (chord-set tonic modes 3))
  ([tonic modes note-ct]
   (mapcat (fn [mode]
             (map #(base-chord (degree %) tonic mode note-ct)
                  (range 1 8)))
           modes)))





