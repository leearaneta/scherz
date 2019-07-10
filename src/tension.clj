(ns scherz.tension
  (:use [overtone.live])
  (:use [overtone.inst.piano])
  (:require [scherz.consonance])
  (:require [scherz.voicing])
  (:require [clojure.data.json :as json])
  (:require [scherz.brightness]))

(refer 'scherz.consonance)
(refer 'scherz.voicing)
(refer 'scherz.brightness)

(defn- normalize [coll]
  (let [cleaned (filter some? coll)
        max-val (apply max cleaned)
        min-val (apply min cleaned)]
    (if (= max-val min-val)
      (map #(when % 0) coll)
      (map #(when % (/ (- % min-val) (- max-val min-val))) coll))))

(defn apply-tension
  ([tension-vecs values target-tension]
   (let [weights (repeat (count tension-vecs) 1)]
     (apply-tension tension-vecs weights values target-tension)))
  
  ([tension-vecs weights values target-tension]
   {:pre (= (count tension-vecs) (count weights) (count values))}
   (let [apply-weight (fn [weight tension-vec]
                        (map #(when % (* weight %)) tension-vec))
         combine-tensions (fn [& args]
                            (reduce #(when (and %1 %2) (+ %1 %2)) args))
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

(defn apply-chord-tension
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
                                                     [1 1 1]
                                                     chords
                                                     target-tension))))
          [(first (chord-set start-tonic modes 4))]
          tension-curve))

(def tension-curve (vec (take 15 (cycle [1/4 1/2 0]))))

(chord-set :Bbb [:lydian])

(def stuff (apply-chord-tension tension-curve :C [:lydian :melodic-minor]))

stuff

(def voiced-stuff (reduce (fn [voiced-chords chord]
                            (conj voiced-chords
                                  (voice-chord (peek voiced-chords)
                                               (:notes chord))))
                          [(map #(+ % 60) (:notes (first stuff)))]
                          (rest stuff)))

(map piano (nth voiced-stuff 15))

; TODO:
  ; extend voice leading algorithm to accomodate chords of different sizes
  ; choose set of possible chords more intelligently
  ; dynamic weights
  ; programatically create weights
  ; create a macro that plays a progression on the beat
  ; add some sick beats

