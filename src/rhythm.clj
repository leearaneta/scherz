(ns scherz.rhythm
  (:use [overtone.live]))

; rhythmic tension
; notes played closer together have more tension

(defn valid-positions [total-subdivisions prev-position]
  (map #(if (> % prev-position) % nil) (range 0 total-subdivisions)))

; notes played closer together have more tension
(defn distance-tension [prev-position
                        subdivisions-per-beat
                        beats-per-measure
                        total-subdivisions]
  (map (fn [position]
         (when position
           (-> prev-position
               (- position)
               (+ (* subdivisions-per-beat beats-per-measure))
               (/ subdivisions-per-beat))))
       (valid-positions total-subdivisions prev-position)))

; syncopated notes have more tension
(defn syncopation-tension [prev-position
                           subdivisions-per-beat
                           total-subdivisions]
  (map (fn [position]
         (when position
           (-> position
               (mod subdivisions-per-beat)
               (/ subdivisions-per-beat)
               (#(if (= % 0) 0 (denominator %))))))
       (valid-positions total-subdivisions prev-position)))



