(ns scherz.gravity
  (:require [scherz.util :refer [abs avg condense]]))

(defn chord-gravity
  "Measures, from 0 to 1, how spatially close two sets of notes are.
  More half step resolutions results in higher gravity."
  [source-notes target-notes]
  (when (not= (condense source-notes) (condense target-notes))
    (->> target-notes
         (map - source-notes)
         (filter (partial not= 0))
         (map abs)
         (map (partial / 1))
         avg)))
