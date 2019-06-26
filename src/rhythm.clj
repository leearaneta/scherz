(ns scherz.rhythm
  (:use [overtone.live]))

; rhythmic tension
; notes played closer together have more tension
(defn valid-positions [current-position]
  (map #(if (> % current-position) % nil) (range 0 320)))

(defn distance-tension [current-position subdivisions-per-beat beats-per-measure]
  (map (fn [position]
         (when position
           (-> current-position
               (- position)
               (+ (* subdivisions-per-beat beats-per-measure))
               (/ subdivisions-per-beat))))
       (valid-positions current-position)))

; syncopated notes have more tension
(defn syncopation-tension [current-position subdivisions-per-beat]
  (map (fn [position]
         (when position
           (-> position
               (mod subdivisions-per-beat)
               (/ subdivisions-per-beat)
               (#(if (= % 0) 0 (denominator %))))))
       (valid-positions current-position)))
