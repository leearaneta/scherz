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
    (play-chord chord)))

(let [scales [:lydian]
      tensions [{:color 0.4 :dissonance 0.25 :gravity 0.2}
                {:color 0.2 :dissonance 0.4 :gravity 0.4}
                {:color 0 :dissonance 0.6 :gravity 0.6}
                {:color 0.15 :dissonance 0.15 :gravity 0.5}]
      options {:root "D" :seed 18}
      progression (generate-progression scales (cycle tensions) options)]
  (play-progression progression))
