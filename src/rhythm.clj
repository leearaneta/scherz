(ns scherz.rhythm
  (:use [overtone.live]))

; rhythmic tension
; notes played closer together have more tension


(defn positions-buffer [total-subdivisions]
  (range 0 (* 2 total-subdivisions)))

(defn valid-positions [total-subdivisions prev-position]
  (map #(if (> % prev-position) % nil)
       (positions-buffer total-subdivisions)))

; notes played closer together have more tension
(defn distance-tension
  [prev-position subdivisions-per-beat beats-per-measure total-subdivisions]
  (map (fn [position]
         (when position
           (-> prev-position
               (- position)
               (+ (* subdivisions-per-beat beats-per-measure))
               (/ subdivisions-per-beat)
               (#(if (neg? %) nil %)))))
       (valid-positions total-subdivisions prev-position)))

; syncopated notes have more tension
(defn syncopation-tension
  [prev-position subdivisions-per-beat total-subdivisions]
  (map (fn [position]
         (when position
           (-> position
               (mod subdivisions-per-beat)
               (/ subdivisions-per-beat)
               (#(if (= % 0) 0 (denominator %))))))
       (valid-positions total-subdivisions prev-position)))

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
