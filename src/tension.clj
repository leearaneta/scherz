(ns scherz.tension
  (:use [overtone.live])
  (:use [overtone.inst.piano])
  (:require [scherz.rhythm])
  (:require [scherz.consonance])
  (:require [scherz.voicing])
  (:require [scherz.brightness]))

(refer 'scherz.rhythm)
(refer 'scherz.consonance)
(refer 'scherz.voicing)
(refer 'scherz.brightness)

(def subdivisions-per-beat 4)
(def beats-per-measure 4)
(def measure-ct 8)
(def total-subdivisions (* subdivisions-per-beat
                           beats-per-measure
                           measure-ct))

(defn absolute-value [n]
  (max n (- n)))

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
         tension-cost (fn [[_ tension]]
                        (if (nil? tension)
                          Integer/MAX_VALUE
                          (absolute-value (- target-tension tension))))]
     (->> tension-vecs
          (map normalize)              ; normalize all tension vectors
          (map apply-weight weights)
          (apply map combine-tensions) ; add them together
          normalize ; normalize again
          (map vector values) ; zip with values
          (min-by tension-cost) ; choose value that matches with target tension
          first))))

(defn apply-rhythm-tension
  [tension-curve subdivisions-per-beat beats-per-measure total-subdivisions]
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
                                       (positions-buffer total-subdivisions)
                                       (tension-curve prev-position))]
      (if (< next-position total-subdivisions)
        (recur (conj positions next-position))
        positions))))

(defn print-return
  ([x] print-return ["" x])
  ([str x]
   (do
     (println str x)
     x)))

; apply tension to chords as well
(defn apply-chord-tension
  [tension-curve positions tonic chord-set]
  (reduce (fn [chords position]
            (let [prev-chord (peek chords)
                  consonance (map chord-consonance chord-set)
                  distances (when prev-chord
                              (->> chord-set
                                   (map (partial chord-distance prev-chord))
                                   (map #(if (< 0 % 4) 0 nil))))
                  tension-vecs (if prev-chord [kt rt ct dt] [kt rt ct])
                  weights (if prev-chord [1/4 1/2 1 1/2] [1/4 1/2 1])]
              (->> (tension-curve position)
                   (apply-tension tension-vecs weights chord-set)
                   (conj chords))))
          []
          positions))

(def chords
  (chord-set :C [:lydian :melodic-minor] 4))

(def rhythm-tension-curve (vec (take total-subdivisions
                                     (cycle [0]))))

(def chord-tension-curve (vec (take total-subdivisions
                                    (cycle [0 1/4 1/8]))))

(def positions (apply-rhythm-tension rhythm-tension-curve
                                     subdivisions-per-beat
                                     beats-per-measure
                                     total-subdivisions))

chords


(def test-chords (apply-chord-tension chord-tension-curve positions :C chords))

(def stuff
  (->> test-chords
       (map (fn [chord]
              (map #(+ 48 %) chord)))))

stuff
(map #(piano %) (nth stuff 7))
(root-tension :C (nth stuff 2))
(euler-gradus (nth stuff 2))

; TODO:
  ; programatically create tension curves



