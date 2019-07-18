(ns scherz.play
  (:use [overtone.inst.piano])
  (:require scherz.generate))

(refer 'scherz.generate)

(defn play-chord [notes]
  (doseq [note notes]
    (piano note)))

(defn play-progression [chords]
  (let [chord-time (->> (iterate (partial + 1000) (now))
                        (map vector (map :notes chords)))]
    (doseq [[notes time] chord-time]
      (at time (play-chord notes)))))

(defn voice [chord]
  (let [to-add (if (< 18 (apply max (:notes chord))) 48 60)]
    (assoc chord :notes (map (partial + to-add) (:notes chord)))))

(let [tensions {:color [0 0.2 0 0.4]
                :consonance [0 0.2 0.6 0.6]
                :gravity [0 0.2 0.2 1]} 
      scales [:lydian :melodic-minor]
      progression (main tensions scales)
      stuff (map voice (generate-progression tensions scales))]
  (play-progression progression)
;  (play-progression stuff)
  stuff
)

; TODO:
  ; redefine gravity (or maybe just omit it?)
    ; the differences it makes are not noticeable and it's annoying to change
  ; fix voice leading and stuff
    ; space notes out more? also avoid having the 7th in the bass
  ; add mood?? and contour (for bass) !!
  ; unit tests
