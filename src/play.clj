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
                        (map vector chords))]
    (doseq [[notes time] chord-time]
      (at time (play-chord notes)))))

(let [tensions {:color [0 0.5 1 1 1 0]
                :consonance [0 0.5 1 1 0 0]
                :gravity [0 0 0.25 0.5 0.5 0]}
      start-tonic :Ab
      modes [:lydian :melodic-minor]
      progression (generate-progression tensions modes start-tonic)]
  (play-progression (voice-progression progression)))
