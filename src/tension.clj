(ns scherz.tension
  (:use [overtone.live])
  (:require [scherz.rhythm])
  (:require [scherz.consonance])
  (:require [scherz.voicing]))

(refer 'scherz.rhythm)
(refer 'scherz.consonance)
(refer 'scherz.voicing)

(def subdivisions-per-beat 4)
(def beats-per-measure 4)
(def measure-ct 8)
(def total-subdivisions (* subdivisions-per-beat
                           beats-per-measure
                           measure-ct))
(def tension-curve (vec (repeat total-subdivisions 0)))

(defn- normalize [coll]
  (let [cleaned (filter some? coll)
        max-val (apply max cleaned)
        min-val (apply min cleaned)]
    (map #(when % (/ (- % min-val) (- max-val min-val))) coll)))

(defn apply-tension
  ([tension-vecs values target-tension]
   (let [weights (repeat (count tension-vecs) 1)]
     (apply-tension tension-vecs weights values target-tension)))
  
  ([tension-vecs weights values target-tension]
   {:pre (= (count tension-vecs) (count weights) (count values))}
   (let [apply-weight (fn [weight tension-vec]
                        (map #(when % (* weight %)) tension-vec))
         combine-tensions (fn [& args]
                            (reduce #(when (and %1 %2) (+ %1 %2)) args))]
     (->> tension-vecs
          (map normalize)              ; normalize all tension vectors
          (map apply-weight weights)   ; apply weights
          (apply map combine-tensions) ; add them together
          normalize                    ; normalize again
          (map vector values)          ; zip with values
          (min-by (fn [[_ tension]] (Math/abs (- target-tension tension))))
          first))))

(defn apply-rhythm-tension [tension-curve
                            subdivisions-per-beat
                            beats-per-measure
                            total-subdivisions]
  (loop [positions [0]]
    (let [prev-position (peek positions)
          dt (distance-tension prev-position
                               subdivisions-per-beat
                               beats-per-measure
                               total-subdivisions)
          st (syncopation-tension prev-position
                                  subdivisions-per-beat
                                  total-subdivisions)
          next-position (apply-tension [dt st]
                                       (range 0 total-subdivisions)
                                       (tension-curve prev-position))]
      (if next-position
        (recur (conj positions next-position))
        positions))))

(apply-rhythm-tension tension-curve
                      subdivisions-per-beat
                      beats-per-measure
                      total-subdivisions)

(def chords
  (chord-set :C [:melodic-minor :lydian] 4))

; apply tension to chords as well
(defn apply-chord-tension [tension-curve
                           positions
                           tonic
                           chord-set
                           total-subdivisions]
  (reduce (fn [chords position]
            (let [prev-chord (peek chords)
                  distances (->> chord-set
                                 (map (partial chord-distance prev-chord))
                                 (map #(when (<= % 4) 0)))
                  key-tension (map (partial key-tension tonic) chord-set)
                  consonance-tension (map euler-gradus chord-set)
                  ; square consonance tension if we're on the down beat
                  ])
            []
            positions)))

