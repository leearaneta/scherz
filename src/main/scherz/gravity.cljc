(ns scherz.gravity
  (:require [scherz.util :refer [min-by abs avg]]))

(defn- compress [notes]
  (sort (map (fn [x] (mod x 12)) notes)))

(defn- invert-asc [notes]
  (sort (cons (+ (first notes) 12)
              (next notes))))

(defn- invert-desc [notes]
  (sort (cons (- (last notes) 12)
              (next (reverse notes)))))

(defn invert
  [notes shift]
  (cond
    (pos? shift) (recur (invert-asc notes) (dec shift))
    (neg? shift) (recur (invert-desc notes) (inc shift))
    (zero? shift) notes))

(defn- gravity
  "Measures, from 0 to 1, how spatially close two chords are given a transition.
  More half step resolutions results in higher gravity."
  [transition]
  (->> transition
       (map second)
       (filter (partial not= 0))
       (map abs)
       (map (partial / 1))
       avg))

(defn- note-distance [current-note target-note]
  (let [diff (- (mod target-note 12)
                (mod current-note 12))]
    (min-by abs [diff (+ diff 12) (- diff 12)])))

(defn- chord-transition
  "Given two sets of notes, shows how much each source note should travel.
  Prioritizes half step resolutions and minimizes leaps."
  [source-notes target-notes]
  (if (= (compress source-notes) (compress target-notes))
    (map vector source-notes (repeat 4 0))

    (->> (range 0 4) ; all chords currently should have four notes
         (map (partial invert target-notes))
         (map (fn [rotation]
                (map note-distance source-notes rotation)))
         (map (fn [distances]
                (map vector source-notes distances)))
         ; TODO: find elegant way to implement max-by
         (min-by (fn [transition] (/ 1 (gravity transition)))))))

(def chord-gravity (comp gravity chord-transition))

(defn voice-lead
  "Applies a chord transition to find an optimal voicing of the target notes."
  [source-notes target-notes]
  (let [transition (chord-transition (compress source-notes)
                                     (compress target-notes))
        mapping (reduce (fn [acc curr]
                          (conj acc {(mod curr 12) curr}))
                        {} source-notes)]
    (sort (map (fn [[k v]]
                 (+ v (mapping k)))
               transition))))

(defn inversion
  "Finds the inversion of a set of notes given the root."
  [root notes]
  (loop [notes notes
         inversion 0]
    (if (-> (first notes) (mod 12) (= root))
      inversion
      (recur (invert-desc notes) (inc inversion)))))
