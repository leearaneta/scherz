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

(defn- inverse-coll
  [coll]
  (map (partial / 1)
       (filter (partial not= 0) coll)))

(defn gravity
  "Measures, from 0 to 1, how spatially close two sets of notes are.
  More half step resolutions results in higher gravity."
  [source-notes target-notes]
  (if (= (count source-notes) (count target-notes))
    (avg (->> target-notes
              (map abs-diff source-notes)
              inverse-coll))
    (let [[four-notes five-notes] (sort-by count [source-notes target-notes])]
      (avg (->> [(conj (vec four-notes) infinity)
                 (conj (apply list four-notes) infinity)]
                (map (partial map abs-diff five-notes))
                (apply map vector)
                (map (partial apply min))
                inverse-coll)))))

(defn sink-octave
  "Brings a set of notes down to the lowest octave possible."
  [notes]
  (if (some #(< % 0) notes)
    notes
    (recur (vec (map #(- % 12) notes)))))

(defn- transfer-chord
  "Moves notes in a chord by the given amount of octaves."
  [notes octave]
  (map (partial + (* octave 12))
       (sink-octave notes)))

(defn transfer-octaves [chord]
  (let [within-range? (fn [notes]
                        (and (<= 40 (first notes))
                             (<= (last notes) 80)))]
    (->> '(3 4 5)
         (map (partial transfer-chord (:notes chord)))
         (filter within-range?)
         (map (partial assoc chord :notes)))))

(defn open-voicings
  "Recursively inverts the tail of a chord to produce \"open\" voicings."
  [invertf coll]
  (let [voice (fn [n]
                (let [inverted (invertf (subvec coll 1) n)]
                  (into [inverted]
                        (if (>= (count inverted) 3)
                          (open-voicings invertf inverted)
                          []))))]
    (->> (range (dec (count coll)))
         (mapcat voice)
         (map (partial into [(first coll)]))
         distinct)))

(defn apply-voicings
  [{:keys [notes pitches] :as chord}]
  (let [note-voicings (open-voicings note-invert notes)
        pitch-voicings (open-voicings pitch-invert pitches)
        update-chord (fn [[new-notes new-pitches]]
                       (assoc chord
                              :notes new-notes
                              :pitches new-pitches))]
    (map update-chord
         (map vector note-voicings pitch-voicings))))
