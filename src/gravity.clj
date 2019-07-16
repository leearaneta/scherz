(ns scherz.gravity
  (:require [scherz.util]))

(refer 'scherz.util)

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
    (min-by absv [diff (+ diff 12) (- diff 12)])))

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
