(ns scherz.generate
  (:require [scherz.util])
  (:require [scherz.consonance])
  (:require [scherz.gravity])
  (:require [scherz.brightness]))

(refer 'scherz.util)
(refer 'scherz.consonance)
(refer 'scherz.gravity)
(refer 'scherz.brightness)

(defn- apply-scores
  "Picks a chord from values that has the lowest combined score."
  [scores values]
  (let [combine-scores (fn [& args] (reduce (fwhen [a b] (+ a b)) args))
        cost (fn [[_ score]]
               (if (nil? score) Integer/MAX_VALUE score))]
    (->> scores
         (apply map combine-scores)
         (map vector values)
         (min-by cost)
         first)))

(defn chord-set
  "Returns all chords within a given tonic / scale."
  [tonic scale]
  (let [note-ct (count (scale-intervals scale))]
    (for [shape (chord-shapes note-ct)
          degree (range 1 (inc note-ct))]
      {:scale scale :tonic tonic
       :root ((pitch-scale tonic scale) (dec degree))
       :pitches (pitch-chord tonic scale shape degree)
       :notes (base-chord tonic scale shape degree)})))

(defn- normalize-consonance
  "Within given scales, normalizes consonance values from 0 to 1 for each chord."
  [scales]
  (let [filtered (select-keys scale-consonance scales)
        consonance-vals (clojure.core/flatten (vals filtered))
        min-consonance (apply min consonance-vals)
        max-consonance (apply max consonance-vals)
        diff (- max-consonance min-consonance)]
    (map-vals (fn [val]
                (map #(-> % (- min-consonance) (/ diff)) val))
              filtered)))

(defn- find-scale
  "Finds a scale that corresponds to a target consonance, and a tonic that
  corresponds to a target color."
  [scales prev-chord target-color target-consonance]
  (let [scale (first (min-by #(abs-diff target-consonance (avg (second %)))
                             (normalize-consonance scales)))
        tonic (->> (scale-brightness scale)
                   (- (scale-brightness (:scale prev-chord)))
                   (+ (Math/round (double (* 5 target-color))))
                   (fifths-above (:tonic prev-chord)))]
    [tonic scale]))

(defn generate-progression
  "Generates a set of chords that matches tension curves within the given scales."
  ([tensions scales] (generate-progression tensions scales :C))
  ([{:keys [col con gra]} scales start-tonic]
   (reductions (fn [prev pos]
                 (let [[targ-col targ-con targ-gra] (map #(% pos) [col con gra])
                       [tonic scale] (find-scale scales prev targ-col targ-con)
                       chords (chord-set tonic scale)
                       col-scores (map (fn [chord]
                                         (->> (:pitches chord)
                                              (chord-color (:pitches prev))
                                              (#(/ % 5)) (abs-diff targ-col)))
                                       chords)
                       con-scores (map (partial abs-diff targ-con)
                                       (scale (normalize-consonance scales)))
                       gra-scores (map #(when-let [g (chord-gravity (:notes prev)
                                                                    (:notes %))]
                                          (max (- targ-gra g) 0))
                                       chords)]
                   (apply-scores [col-scores con-scores gra-scores] chords)))
               (first (chord-set start-tonic (first scales)))
               (range 0 (count col)))))

(defn voice-progression
  "Implement proper voice leading in a progression.
  If any note is below midi 60 or above midi 80, invert the chord."
  [progression]
  (let [constrain-voicing (fn [notes]
                            (cond (< (apply min notes) 60) (invert notes 1)
                                  (< 80 (apply max notes)) (invert notes -1)
                                  :else notes))
        voice-notes (fn [prev-chord chord]
                      (->> (:notes prev-chord)
                           (#(voice-lead % (:notes chord)))
                           constrain-voicing
                           (assoc chord :notes)))
        voice-pitches (fn [chord]
                        (->> (:notes chord)
                             (inversion (pitch->midi (:root chord)))
                             (rotate (:pitches chord))
                             (assoc chord :pitches)))
        initial-chord (->> (:notes (first progression))
                           (map (partial + 60))
                           constrain-voicing
                           (assoc (first progression) :notes)
                           voice-pitches)]
    (reductions (comp voice-pitches voice-notes)
                initial-chord
                (rest progression))))

(def main (comp voice-progression generate-progression))
