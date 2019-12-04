(ns scherz.gravity
  (:require [scherz.util :refer [avg abs-diff]]))

(defn condense
  ([notes] (condense notes 12))
  ([notes n]
   (distinct (map #(mod % n) notes))))

(defn note-invert
  "(note-invert '(0 4 7 11) 2) -> '(7 11 12 16)"
  [notes shift]
  (let [first-note (if (= (count (condense notes)) 3)
                     (+ (second notes) 12)
                     (+ (first notes) 12))]
    (if (zero? shift)
      notes
      (recur (->> first-note
                  (conj (vec (next notes)))
                  (apply list))
             (dec shift)))))

(defn pitch-invert
  "(pitch-invert '(\"C\" \"E\" \"G\" \"C\")) -> '(\"E\" \"G\" \"C\" \"E\")"
  [pitches shift]
  (let [first-pitch (if (= (count (distinct pitches)) 3)
                      (second pitches)
                      (first pitches))]
    (if (zero? shift)
      pitches
      (recur (->> first-pitch
                  (conj (vec (next pitches)))
                  (apply list))
             (dec shift)))))

(defn chord-gravity
  "Measures, from 0 to 1, how spatially close two sets of notes are.
  More half step resolutions results in higher gravity."
  [source-notes target-notes]
  (when (not= (condense source-notes) (condense target-notes))
    (->> target-notes
         (map abs-diff source-notes)
         (filter (partial not= 0))
         (map (partial / 1))
         avg)))
