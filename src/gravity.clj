(ns scherz.gravity
  (:use [overtone.live]))

(def degree->roman
  (zipmap (vals DEGREE) (keys DEGREE)))

(defn- adjust-tonic [tonic]
  (if (< 1 (count (name tonic)))
    (let [tonic (name tonic)
          multiplier (if (= \# (last tonic)) 1 -1)]
      (-> tonic count dec
          (* multiplier)
          (+ (-> tonic first str keyword NOTES))
          (mod 12) REVERSE-NOTES))
    tonic))

(defn base-chord [tonic mode note-ct degree]
  (chord-degree (degree->roman degree)
                (-> tonic adjust-tonic name (str -1) keyword)
                mode
                note-ct))

(defn compress [notes]
  (distinct (map (fn [x] (mod x 12)) notes)))

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
  (if (= (sort (compress source-notes)) (sort (compress target-notes)))
    (map vector source-notes (repeat (count source-notes) 0))
    (->> (range 0 (count target-notes))
         (map (partial rotate-chord target-notes))
         (map (fn [rotation]
                (map note-distance source-notes rotation)))
         (map (fn [distances]
                (map vector source-notes distances)))
         (min-by gravity))))

(def chord-gravity (comp gravity chord-transition))

(defn voice-chord [source-notes target-notes]
  (map (fn [[k v]] (+ k v))
       (chord-transition source-notes target-notes)))
