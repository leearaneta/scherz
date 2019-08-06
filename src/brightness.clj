(ns scherz.brightness
  (:require [clojure.string])
  (:require [scherz.util]))

(defn valid-direction? [direction]
  (or (= direction :asc) (= direction :desc)))

(defn shift-pitch
  "Sharpens or flattens a pitch based on direction."
  ([direction pitch] (shift-pitch direction pitch 1))
  ([direction pitch amt]
   {:pre [(valid-direction? direction)]}
   (if (== amt 0) (keyword pitch)
       (let [pitch (name pitch)
             pop-string (fn [s] (subs s 0 (- (count s) 1)))
             to-remove (if (= direction :asc) "b" "#")
             to-add (if (= direction :asc) "#" "b")]
         (if (clojure.string/ends-with? pitch to-remove)
           (recur direction (pop-string pitch) (- amt 1))
           (recur direction (str pitch to-add) (- amt 1)))))))

(def sharpen (partial shift-pitch :asc))
(def flatten (partial shift-pitch :desc))

(def base-circle [\F \C \G \D \A \E \B])

(defn fifths
  "Creates an infinite sequence of fifths (ascending or descending)."
  ([root] (fifths root :asc))
  ([root direction]
   {:pre [(valid-direction? direction)]}
   (let [base-fifths (if (= direction :asc)
                       base-circle
                       (reverse base-circle))
         shift-fn (if (= direction :asc) sharpen flatten)
         natural-root (first (name root))
         initial-accidental (subs (name root) 1)
         shift (fn [[index value]]
                 (->> (/ index 7)
                      (#(Math/floor %))
                      (shift-fn value)))]
     (->> base-fifths
          (map #(keyword (str % initial-accidental)))
          cycle
          (map vector (range))
          (map shift)
          (drop (.indexOf base-fifths natural-root))))))

(defn scale-brightness
  "Assigns each note in a scale a level of brightness based on its position in the
  circle of fifths relative to the root, and adds them all up.  The tritone can be
  -6 or 6, and is inferred based on the brightness of the rest of the scale."
  [scale]
  (let [cumulative-intervals (reductions + (scherz.util/scale-intervals scale))
        note-ct (count cumulative-intervals)
        interval-brightness [0 -5 2 -3 4 -1 0 1 -4 3 -2 5 0]
        scale-brightness (scherz.util/avg (map interval-brightness
                                               cumulative-intervals))]
    (if (some #(= 6 %) cumulative-intervals)
      (if (pos? scale-brightness)
        (+ scale-brightness (/ 6 note-ct))
        (- scale-brightness (/ 6 note-ct)))
      scale-brightness)))

(defn circle-of-fifths
  "Generates a circle of fifths given a root and a scale.
  If the scale is bright the tritone is placed above the root, otherwise below."
  ([root] (circle-of-fifths root :major))
  ([root scale]
   (let [bright? (pos? (scale-brightness scale))
         upper-arc (take (if bright? 6 5)
                         (drop 1 (fifths root)))
         lower-arc (take (if bright? 6 7)
                         (fifths root :desc))]
     (into upper-arc lower-arc))))

(defn pitch-scale
  "Returns all pitches in a scale based on the circle of fifths.
  (pitch-scale :C :locrian) -> [:C :Db :Eb :F :Gb :Ab :Bb]"
  [tonic scale]
  (let [circle (vec (circle-of-fifths tonic scale))
        root-index (.indexOf circle tonic)
        intervals (scherz.util/scale-intervals scale)]
    (->> intervals
         (reductions +)
         (map #(-> % (* 7) (+ root-index) (mod 12)))
         (mapv circle)
         pop
         (into [tonic]))))

(defn pitch-chord
  "(pitch-chord :C :major [0 2 4] 2) -> (:D :F :A)"
  [tonic scale chord-shape degree]
  (->> (pitch-scale tonic scale)
       cycle
       (drop (dec degree))
       (take (inc (last chord-shape)))
       (#(map (vec %) chord-shape))))

(defn pitch-brightness
  "Measures a pitch's brightness based on its position in the circle of fifths.
  More useful as a relative measure - arbitrarily F has a brightness of 0."
  [pitch]
  (let [p (name pitch)
        counts (frequencies p)]
    (+ (.indexOf base-circle (first p))
       (* 7 (get counts \# 0))
       (* -7 (get counts \b 0)))))

(defn chord-color
  "Computes how much more 'colorful' chords are in relation to each other.

  The C Major triad's brightest note is E and the G major triad's brightest is B. 
  B is one level brighter than E in the circle of fifths, so
  '(:G :B :E) adds one unit of color to '(:C :E :G)."
  [source-pitches target-pitches]
  (let [chord-brightness (fn [pitches]
                           (map pitch-brightness pitches))
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
  [root n]
  (->> (if (pos? n) :asc :desc)
       (fifths root)
       (drop (scherz.util/abs n))
       first))
