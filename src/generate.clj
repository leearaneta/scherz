(ns scherz.generate
  (:use [overtone.live])
  (:use [overtone.inst.piano])
  (:require [scherz.consonance])
  (:require [scherz.gravity])
  (:require [scherz.brightness]))

(refer 'scherz.consonance)
(refer 'scherz.gravity)
(refer 'scherz.brightness)

(defmacro fwhen [args body]
  (let [new-body `(if (some nil? ~args) nil ~body)]
    `(fn ~args ~new-body)))

(defn- normalize [coll]
  (let [cleaned (filter some? coll)
        max-val (apply max cleaned)
        min-val (apply min cleaned)]
    (map (fwhen [v] (/ (- v min-val) (- max-val min-val))) coll)))

(defn- apply-tension
  ([tension-vecs values target-tension]
   (let [weights (repeat (count tension-vecs) 1)]
     (apply-tension tension-vecs weights values target-tension)))
  
  ([tension-vecs weights values target-tension]
   {:pre (= (count tension-vecs) (count weights))}
   (let [apply-weight (fn [weight tension-vec]
                        (map (fwhen [tension] (* weight tension)) tension-vec))
         combine-tensions (fn [& args]
                            (reduce (fwhen [a b] (+ a b)) args))
         abs #(max % (- %))
         tension-cost (fn [[_ tension]]
                        (if (nil? tension)
                          Integer/MAX_VALUE
                          (abs (- target-tension tension))))]
     (->> tension-vecs
          (map normalize) ; normalize all tension vectors
          (map apply-weight weights)
          (apply map combine-tensions) ; add them together
          normalize ; normalize again
          (map vector values) ; zip with values
          (min-by tension-cost) ; choose value that matches with target tension
          first))))

(defn- apply-tensions
  [tension-vecs values target-tensions]
  {:pre (= (count tension-vecs) (count target-tensions))}
  (let [abs #(max % (- %))
        score (fn [target-tension tension-vec]
                (map (fwhen [t] (abs (- target-tension t))) tension-vec))
        combine-tensions (fn [& args]
                            (reduce (fwhen [a b] (+ a b)) args))
        cost (fn [[_ score]]
               (if (nil? score) Integer/MAX_VALUE score))]
    (->> tension-vecs
         (map normalize)
         (map score target-tensions)
         (apply map combine-tensions)
         (map vector values)
         (min-by cost)
         first)))

(defn chord-set
  ([tonic modes] (chord-set tonic modes 4))
  ([tonic modes note-ct]
   (for [tonic [tonic (second (fifths tonic)) (second (fifths tonic :desc))]
         mode modes
         degree (range 1 8)]
     {:tonic tonic :mode mode
      :root ((pitch-scale tonic mode) (dec degree))
      :pitches (pitch-chord tonic mode note-ct degree)
      :notes (base-chord tonic mode note-ct degree)})))

(defn generate-chords
  [tension-curve start-tonic modes]
  (reduce (fn [chord-progression target-tension]
            (let [prev-chord (peek chord-progression)
                  chords (chord-set (:tonic prev-chord) modes)
                  consonance (map #(chord-consonance (:notes %)) chords)
                  gravity (map #(chord-gravity (:notes prev-chord) (:notes %))
                               chords)
                  color (map #(chord-color (:pitches prev-chord) (:pitches %))
                             chords)]
              (conj chord-progression (apply-tension [consonance gravity color]
                                                     chords
                                                     target-tension))))
          [(first (chord-set start-tonic modes 4))]
          tension-curve))

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
        initial-voicing (->> initial-chord second (+ 12)
                             (assoc (vec initial-chord) 1)
                             (map (partial + 60)) sort)]
    (reduce (fn [voiced-progression chord]
              (conj voiced-progression
                    (sort (adjust-voicing (voice-lead (peek voiced-progression)
                                                      (:notes chord))))))
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

(def tension-curve (vec (take 18 (cycle [1/2 1 0]))))

(def progression (generate-chords tension-curve :C [:lydian :melodic-minor]))

(play-progression (voice-progression progression))

; TODO:
  ; extend voice leading algorithm to accomodate chords of different sizes
  ; choose set of possible chords more intelligently
  ; programatically create tension curves
