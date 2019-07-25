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

(play-chord '(60 64 67))

; dissonant - how unnatural a chord sounds
; not to be confused with "happy" or "sad"
(play-chord '(60 63 67 71))

; colorful - how many new notes are introduced from the previous scale
(play-chord '(63 67 70))

; gravity - spatial closeness to the previous chord
; higher gravity means more attraction to previous chord
(play-chord '(60 65 68))

(let [tensions {:color []
                :dissonance []
                :gravity []}
      scales [:major]
      progression (main tensions scales)]
  (play-progression progression)
  (map #(select-keys % [:notes :pitches :type])
       progression))

; TODO:
    ; avoid having the 7th in the bass
    ; pass in a random seed
    ; use transducers?? bu t'where
    ; unit tests
