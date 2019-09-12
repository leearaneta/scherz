(ns scherz.gravity
  (:require [scherz.util :refer [abs avg]]))

(defn condense
  ([notes] (condense notes 12))
  ([notes n]
   (distinct (map #(mod % n) notes))))

(defn- invert
  [flast coll shift]
  (if (zero? shift)
    coll
    (recur flast
           (->> (flast coll)
                (conj (vec (next coll)))
                (apply list))
           (dec shift))))

(def note-invert
  (let [f (fn [notes]
            (if (= (count (condense notes)) 3)
              (+ (second notes) 12)
              (+ (first notes) 12)))]
    (partial invert f)))

(def pitch-invert
  (let [f (fn [pitches]
            (if (= (count (distinct pitches)) 3)
              (second pitches)
              (first pitches)))]
    (partial invert f)))

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
