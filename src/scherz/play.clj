(ns scherz.play
  (:use [overtone.live]
        [overtone.inst.piano])
  (:require [scherz.generate :refer [generate-progression]]))

(defn play-chord [chord]
  (doseq [note (map (partial + 0) (:notes chord))]
    (piano note)))

(defn play-progression [progression]
  (doseq [chord progression]
    (println chord)
    (play-chord chord)
    (Thread/sleep 750)))

(let [scales [:diminished]
      tensions [{:color 0 :dissonance 0.2 :gravity 0.5}
                {:color 0.4 :dissonance 0.4 :gravity 0.25}
                {:color 0 :dissonance 0.8 :gravity 0}
                {:color 0.2 :dissonance 0.2 :gravity 0.25}]
      options {:tonic "F#" :seed 6}
      progression (generate-progression scales tensions options)]
  (play-progression progression))
