(ns scherz.voicing
  (:use [overtone.live])
  (:require [scherz.brightness]))

(def degree->roman
  (zipmap (vals DEGREE) (keys DEGREE)))

(defn base-chord [tonic mode note-ct degree]
  (chord-degree (degree->roman degree)
                (-> tonic name (str -1) keyword)
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

(defn note-distance- [current-note target-note]
  (let [diff (- (mod target-note 12)
                (mod current-note 12))]
    (min-by #(Math/abs %)
            [diff (+ diff 12) (- diff 12)])))


; TODO: allow this to handle chords of different lengths
; maybe sort?
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

(defn chord-distance [source-notes target-notes]
  (reduce (fn [total [_ distance]]
            (+ total (Math/abs distance)))
          0 (chord-transition source-notes target-notes)))

(defn pitch-chord [tonic mode note-ct degree]
  (->> (scherz.brightness/pitch-scale tonic mode)
       cycle
       (drop (dec degree))
       (take-nth 2)
       (take note-ct)))
