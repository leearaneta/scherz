(ns scherz.gravity
  (:require [scherz.util :refer [abs avg]]))

(defn compress
  [notes]
  (if (some #(< % 12) notes)
    notes
    (recur (map #(- % 12) notes))))


(defn chord-gravity
  "Measures, from 0 to 1, how spatially close two sets of notes are.
  More half step resolutions results in higher gravity."
  [source-notes target-notes]
  (let [compress (fn [notes] (sort (distinct (map #(mod % 12) notes))))]
    (if (= (compress source-notes) (compress target-notes))
      nil
      (->> target-notes
           (map - source-notes)
           (filter (partial not= 0))
           (map abs)
           (map (partial / 1))
           avg))))
