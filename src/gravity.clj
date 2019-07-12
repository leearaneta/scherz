(ns scherz.gravity
  (:use [overtone.live]))

(def degree->roman
  (zipmap (vals DEGREE) (keys DEGREE)))

(defn adjust-pitch [pitch]
  (if (< 1 (count (name pitch)))
    (let [pitch (name pitch)
          multiplier (if (= \# (last pitch)) 1 -1)]
      (-> pitch count dec
          (* multiplier)
          (+ (-> pitch first str keyword NOTES))
          (mod 12) REVERSE-NOTES))
    pitch))

(defn base-chord [tonic mode note-ct degree]
  (chord-degree (degree->roman degree)
                (-> tonic adjust-pitch name (str -1) keyword)
                mode
                note-ct))

(defn compress [notes]
  (sort (map (fn [x] (mod x 12)) notes)))

(def compressed-chord (comp compress base-chord))

(defn min-by [f coll]
  (:elem (reduce (fn [acc val]
                   (let [compare (f val)]
                     (if (< compare (:min acc))
                       {:elem val :min compare}
                       acc)))
                 {:elem (first coll) :min (f (first coll))}
                 (rest coll))))

(defn- rotate-chord
  ([notes] (rotate-chord notes 1))
  ([notes offset] (->> (cycle notes)
                       (drop offset)
                       (take (count notes)))))

(defn- gravity [transition]
  (let [filtered (filter (fn [[_ v]] (not= v 0)) transition)
        square #(* % %)]
    (when (seq filtered)
      (/ (->> filtered (map (fn [[_ v]] (square v))) (reduce +))
         (count filtered)))))

(defn- note-distance [current-note target-note]
  (let [diff (- (mod target-note 12)
                (mod current-note 12))]
    (min-by #(Math/abs %)
            [diff (+ diff 12) (- diff 12)])))

(defn- chord-transition [source-notes target-notes]
  (if (= (compress source-notes) (compress target-notes))
    (map vector source-notes (repeat (count source-notes) 0))
    (->> (range 0 (count target-notes))
         (map (partial rotate-chord target-notes))
         (map (fn [rotation]
                (map note-distance source-notes rotation)))
         (map (fn [distances]
                (map vector source-notes distances)))
         (min-by gravity))))


(def chord-gravity (comp gravity chord-transition))

(defn voice-lead [source-notes target-notes]
  (let [transition (chord-transition (compress source-notes)
                                     (compress target-notes))
        mapping (reduce (fn [acc curr]
                          (conj acc {(mod curr 12) curr}))
                        {} source-notes)]
    (sort (map (fn [[k v]]
                 (+ v (mapping k)))
               transition))))
