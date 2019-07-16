(ns scherz.play
  (:use [overtone.live])
  (:use [overtone.inst.piano])
  (:require scherz.gravity)
  (:require scherz.generate))

(defn- invert-asc [notes]
  (sort (cons (+ (first notes) 12)
              (next notes))))

(defn- invert-desc [notes]
  (sort (cons (- (last notes) 12)
              (next (reverse notes)))))

(defn invert-voicing [notes shift]
  (cond
    (pos? shift) (recur (invert-asc notes) (dec shift))
    (neg? shift) (recur (invert-desc notes) (inc shift))
    (zero? shift) notes))

(defn voice-progression [progression]
  (let [adjust-voicing (fn [notes]
                         (cond (< (apply min notes) 60) (invert-voicing notes 1)
                               (< 78 (apply max notes)) (invert-voicing notes -1)
                               :else notes))
        initial-chord (:notes (first progression))
        initial-voicing (->> (second initial-chord) (+ 12)
                             (assoc (vec initial-chord) 1)
                             (map (partial + 60)) sort)]
    (reduce (fn [voiced-progression chord]
              (conj voiced-progression
                    (-> (peek voiced-progression)
                        (scherz.gravity/voice-lead (:notes chord))
                        adjust-voicing sort)))
            [initial-voicing]
            (rest progression))))

(defn play-chord [notes]
  (doseq [note notes]
    (piano note)))

(defn play-progression [chords]
  (let [chord-time (->> (iterate (partial + 1000) (now))
                        (map vector chords))]
    (doseq [[notes time] chord-time]
      (at time (play-chord notes)))))

(let [tensions {:color [0 0.5 1 1 1 0.5]
                :consonance [0 0.5 1 1 0 0]
                :gravity [0 0 0.5 0.5 0.5 0]}
      start-tonic :C
      modes [:lydian :melodic-minor]
      progression (scherz.generate/generate-chords tensions modes start-tonic)]
  (play-progression (voice-progression progression)))
