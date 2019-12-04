(ns scherz.brightness
  (:require [clojure.string :refer [join]]
            [scherz.util :refer [abs rotate]]
            [scherz.scale :refer [scale-intervals scales valid-scale?]]))

(defn valid-direction? [direction]
  (or (= direction :asc) (= direction :desc)))

(def base-circle [\F \C \G \D \A \E \B])

(def pitch-indexes
  (reduce-kv (fn [acc index pitch] (into acc {pitch index}))
             {} base-circle))

(defn valid-pitch? [pitch]
  (let [accidentals (subs pitch 1)]
    (and (some (partial = (first pitch)) base-circle)
         (-> accidentals distinct count (<= 1))
         (some (partial = (first accidentals)) '(\# \b nil)))))

(defn pitch->brightness
  "Measures a pitch's brightness based on its position in the circle of fifths.
  More useful as a relative measure - arbitrarily F has a brightness of 0."
  [pitch]
  (let [counts (frequencies pitch)]
    (+ (pitch-indexes (first pitch)) ; get base brightness from letter of pitch
       (* 7 (get counts \# 0)) ; for every sharp, add 7 to brightness
       (* -7 (get counts \b 0))))) ; same with flats

(defn brightness->pitch [brightness]
  (let [base-pitch (base-circle (mod brightness 7))
        base-accidental (if (pos? brightness) "#" "b")
        accidental-ct (abs (Math/floor (/ brightness 7)))]
    (->> (repeat accidental-ct base-accidental) join (str base-pitch))))

(defn fifths
  "Creates an infinite sequence of fifths (ascending or descending)."
  ([pitch] (fifths pitch :asc))
  ([pitch direction]
   {:pre [(valid-direction? direction)]}   
   (let [f (if (= direction :asc) inc dec)]
     (iterate (comp brightness->pitch f pitch->brightness) pitch))))

(def scale-brightness
  "Mapping of scale -> brightness, which is calculated from the scale's intervals."
  (->> scales
       (map (fn [scale]
              (let [intervals (reductions + (scale-intervals scale))
                    interval-brightness [0 -5 2 -3 4 -1 0 1 -4 3 -2 5 0]
                    base-scale-brightness (->> (map interval-brightness intervals)
                                               (reduce +))
                    ; tritone brightness inferred from brightness of rest of scale
                    tritone-brightness (cond
                                         (not-any? (partial = 6) intervals) 0
                                         (pos? base-scale-brightness) 6
                                         :else -6)]
                (/ (+ base-scale-brightness tritone-brightness)
                   (count intervals)))))
       (map vector scales)
       (into {})))

(defn circle-of-fifths
  "Generates a circle of fifths given a tonic and a scale."
  ([tonic] (circle-of-fifths tonic :major))
  ([tonic scale]
   {:pre [(valid-pitch? tonic) (valid-scale? scale)]}
   (let [bright? (pos? (scale-brightness scale))
         ; if the scale is bright, place the tritone in the upper arc of the circle
         upper-arc (take (if bright? 6 5)
                         (drop 1 (fifths tonic)))
         lower-arc (take (if bright? 6 7)
                         (fifths tonic :desc))]
     (into upper-arc lower-arc))))

(defn pitch-scale
  "Returns all pitches in a scale."
  [tonic scale]
  (let [circle (vec (circle-of-fifths tonic scale))
        root-index (.indexOf circle tonic)
        note-index (fn [interval]
                     (-> interval (* 7) (+ root-index) (mod 12)))]
    (->> (scale-intervals scale)
         (reductions +) ; get cumulative scale intervals
         (map note-index) ; find each note's index in the circle of fifths
         (mapv circle) ; apply indices to circle
         rotate)))

(defn pitch-chord
  "(pitch-chord \"C\" :major [0 2 4] 2) -> (\"D\" \"F\" \"A\")"
  [tonic scale chord-shape degree]
  (->> (cycle (pitch-scale tonic scale))
       (drop (dec degree)) ; start scale from nth degree - 1
       (take (inc (last chord-shape))) ; trim scale
       (#(map (vec %) chord-shape)))) ; apply chord shape to scale

(defn chord-color
  "Computes how much more 'colorful' chords are in relation to each other.

  The C Major triad's brightest note is E and the G major triad's brightest is B. 
  B is one level brighter than E in the circle of fifths, so
  '(\"G\" \"B\" \"E\") adds one unit of color to '(\"C\" \"E\" \"G\")."
  [source-pitches target-pitches]
  (let [chord-brightness (fn [pitches]
                           (map pitch->brightness pitches))
        brightest-note (fn [pitches]
                         (apply max (chord-brightness pitches)))
        darkest-note (fn [pitches]
                       (apply min (chord-brightness pitches)))
        brightness-difference (- (brightest-note target-pitches)
                                 (brightest-note source-pitches))
        darkness-difference (- (darkest-note source-pitches)
                               (darkest-note target-pitches))]
    (+ (max brightness-difference 0)
       (max darkness-difference 0))))

(defn fifths-above
  [n root]
  (->> (pitch->brightness root)
       (+ n)
       brightness->pitch))

(defn fifths-between [source-pitch target-pitch]
  (let [diff (- (pitch->brightness target-pitch)
                (pitch->brightness source-pitch))
        direction (if (pos? diff) :asc :desc)]
    (take (inc diff) (fifths source-pitch direction))))
