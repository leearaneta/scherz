(ns scherz.play
  (:use [overtone.live])
  (:use [overtone.inst.piano])
  (:require scherz.generate))

(refer 'scherz.generate)

(defn play-chord [notes]
  (doseq [note (dedupe notes)]
    (piano note)))

(defn play-progression [chords]
  (let [chord-time (->> (iterate (partial + 1000) (now))
                        (map vector (map :notes chords)))]
    (doseq [[notes time] chord-time]
      (at time (play-chord notes)))))

(let [tensions {:color [0 0 0 0.2]
                :dissonance [0 0.4 0.75 0.1]
                :gravity [0 0 0 0]}
      scales [:major :harmonic-minor]
      progression (main tensions scales)]
  (play-progression progression)
  progression)

; TODO:
    ; avoid having the 7th in the bass
    ; pass in a random seed
    ; use transducers and stuff
    ; unit tests
    ; use lazy-seq macro instead of cycle to create infinite fifths

