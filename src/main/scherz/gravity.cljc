(ns scherz.gravity
  (:require [scherz.util :refer [avg abs-diff infinity]]))

(defn condense
  ([notes] (condense notes 12))
  ([notes n]
   (distinct (map #(mod % n) notes))))

(defn note-invert
  "(note-invert [0 4 7 11] 2) -> [7 11 12 16]"
  [notes shift]
  (if (zero? shift)
    notes
    (recur (conj (subvec notes 1) (+ (first notes) 12))
           (dec shift))))

(defn pitch-invert
  "(pitch-invert [\"C\" \"E\" \"G\" \"C\"] 1) -> [\"E\" \"G\" \"C\" \"E\"]"
  [pitches shift]
  (if (zero? shift)
    pitches
    (recur (conj (subvec pitches 1) (first pitches))
           (dec shift))))

(defn chord-gravity
  "Measures, from 0 to 1, how spatially close two sets of notes are.
  More half step resolutions results in higher gravity."
  [source-notes target-notes]
  (if (= (count source-notes) (count target-notes))
    (avg (->> target-notes
              (map abs-diff source-notes)
              (filter (partial not= 0))
              (map (partial / 1))))
    (let [[four-notes five-notes] (sort-by count [source-notes target-notes])]
      (avg (->> [(conj (vec four-notes) infinity) (conj four-notes infinity)]
                (map (fn [notes] (map abs-diff five-notes notes)))
                (apply map vector)
                (map (partial apply min))
                (filter (partial not= 0))
                (map (partial / 1)))))))

(defn sink-octave
  "Brings a set of notes down to the lowest octave possible."
  [notes]
  (if (some #(< % 12) notes)
    notes
    (recur (map #(- % 12) notes))))

(defn- transfer-chord
  "Moves notes in a chord by the given amount of octaves."
  [notes octave]
  (map (partial + (* octave 12))
       (sink-octave notes)))

(defn transfer-octaves [chord]
  (->> '(3 4 5)
       (map (partial transfer-chord (:notes chord)))
       (filter (fn [notes]
                 (and (<= 38 (first notes))
                      (<= (last notes) 82))))
       (map (partial assoc chord :notes))))

(defn open-voicing
  "Raises the second note of a chord up an octave."
  [chord]
  (let [voice-notes (fn [notes]
                      (if (= (count notes) 4)
                        (-> notes
                            (assoc 1 (notes 2))
                            (assoc 2 (notes 3))
                            (assoc 3 (+ 12 (notes 1))))
                        (-> notes
                            (assoc 1 (notes 2))
                            (assoc 2 (+ 12 (notes 1))))))
        voice-pitches (fn [pitches]
                        (if (= (count pitches) 4)
                          (-> pitches
                              (assoc 1 (pitches 2))
                              (assoc 2 (pitches 3))
                              (assoc 3 (pitches 1)))
                          (-> pitches
                              (assoc 1 (pitches 2))
                              (assoc 2 (pitches 1)))))]
    (-> chord
        (update :notes voice-notes)
        (update :pitches voice-pitches))))
