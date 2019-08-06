(ns scherz.play
  (:use [overtone.live])
  (:use [overtone.inst.piano])
  (:require [clojure.data.json :as json])
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

(let [color [0 0.4 0 0]
      dissonance [0 0.5 0.8 0]
      gravity [0 0 0 0]
      tensions (map (fn [c d g] {:color c :dissonance d :gravity g})
                    color dissonance gravity)
      tonic :C
      scales [:major :diminished]
      progression (main tensions scales tonic)]
  (play-progression progression)

  tensions)


;; (json/write-str
 ;; {:chords (map #(select-keys % [:notes :pitches :type]) progression)
  ;; :tensions tensions})



(resolve-chord :minor)
(chord :F3 :major)

; TODO:
    ; avoid having the 7th in the bass
    ; pass in a random seed
    ; use transducers?? bu t'where
    ; unit tests
