(ns scherz.gravity
  (:require [scherz.util :refer [abs avg]]))

(defn compress [notes]
  ; recursively subtract 12 from every note
  (if (some #(< % 12) notes)
    notes
    (map #(- % 12) notes)))

(defn- invert-asc [notes]
  (sort (cons (+ (first notes) 12)
              (next notes))))

(defn- invert-desc [notes]
  (sort (cons (- (last notes) 12)
              (next (reverse notes)))))

(defn invert
  [notes shift]
  (cond
    (zero? shift) notes
    (pos? shift) (recur (invert-asc notes) (dec shift))
    (neg? shift) (recur (invert-desc notes) (inc shift))))

(defn chord-gravity
  "Measures, from 0 to 1, how spatially close two chords are given a transition.
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

(defn inversion
  "Finds the inversion of a set of notes given the root."
  [root notes]
  (loop [notes notes
         inversion 0]
    (if (-> (first notes) (mod 12) (= root))
      inversion
      (recur (invert-desc notes) (inc inversion)))))
