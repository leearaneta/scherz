(ns scherz.play
  (:use [overtone.live]
        [overtone.inst.piano])
  (:require [scherz.generate :refer [generate-progression]]))

(defn play-chord [chord]
  (doseq [note (:notes chord)]
    (piano note)))

(defn play-progression [progression]
  (doseq [chord progression]
    (println chord)
    (play-chord chord)
    (Thread/sleep 1000)))

(let [scales [:lydian]
      tensions [{:color 0.4 :dissonance 0.6 :gravity 0.2}
                {:color 0.2 :dissonance 0.8 :gravity 0}
                {:color 0 :dissonance 1 :gravity 0}
                {:color 0.15 :dissonance 0.15 :gravity 0.15}]
      options {:root "C" :type "M7" :seed 0}
      progression (generate-progression scales tensions options)]
  (play-progression progression))
