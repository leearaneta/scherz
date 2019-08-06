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

; dissonant - how unnatural a chord sounds
; not to be confused with "happy" or "sad"

; colorful - how many new notes are introduced from the previous scale

; gravity - spatial closeness to the previous chord
; higher gravity means more attraction to previous chord
