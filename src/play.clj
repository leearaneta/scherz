(ns scherz.play
  (:use [overtone.live])
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

(let [tensions {:col [0 0 0.4 0.2 0.4 0.5]
                :con [0 1 0 0.5 0.5 0]
                :gra [0 0 0 0 0 0]}
      scales [:lydian :melodic-minor]
      progression (main tensions scales)
      stuff (map voice (generate-progression tensions scales))]
  (play-progression progression)
  progression)

; TODO:
    ; go over gravity stuff
    ; space notes out more? also avoid having the 7th in the bass
    ; find scales based on consonance more effectively
    ; use transducers and stuff
    ; unit tests
    ; use lazy-seq macro instead of cycle to create infinite fifths
