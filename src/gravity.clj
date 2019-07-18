(ns scherz.gravity
  (:require [scherz.util]))

(refer 'scherz.util)

(defn- compress [notes]
  (sort (map (fn [x] (mod x 12)) notes)))

(defn- invert-asc [notes]
  (sort (cons (+ (first notes) 12)
              (next notes))))

(defn- invert-desc [notes]
  (sort (cons (- (last notes) 12)
              (next (reverse notes)))))

(defn invert [notes shift]
  (cond
    (pos? shift) (recur (invert-asc notes) (dec shift))
    (neg? shift) (recur (invert-desc notes) (inc shift))
    (zero? shift) notes))

(defn- gravity [transition]
  (let [filtered (filter (fn [[_ v]] (not= v 0)) transition)
        square #(* % %)]
    (when (seq filtered)
      (/ (->> filtered (map (fn [[_ v]] (square v))) (reduce +))
         (count filtered)))))

(defn- note-distance [current-note target-note]
  (let [diff (- (mod target-note 12)
                (mod current-note 12))]
    (min-by abs [diff (+ diff 12) (- diff 12)])))

(defn- chord-transition [source-notes target-notes]
  (if (= (compress source-notes) (compress target-notes))
    (map vector source-notes (repeat 4 0))

    (->> (range 0 4) ; all chords currently should have four notes
         (map (partial invert target-notes))
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

(defn inversion [root notes]
  (loop [notes notes
         inversion 0]
    (if (-> (first notes) (mod 12) (= root))
      inversion
      (recur (invert-desc notes) (inc inversion)))))


; TODO: add measure of bass contour
