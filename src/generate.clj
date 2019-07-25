(ns scherz.generate
  (:require [scherz.util])
  (:require [scherz.dissonance])
  (:require [scherz.gravity])
  (:require [scherz.brightness]))

(refer 'scherz.util)
(refer 'scherz.dissonance)
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
  (let [note-ct (count (scale-intervals scale))
        combine-keywords (fn [& keywords]
                           (if (some nil? keywords)
                             nil
                             (->> keywords (map name) (apply str) keyword)))]
    (for [shape (chord-shapes note-ct)
          degree (range 1 (inc note-ct))]
      (let [notes (base-chord tonic scale shape degree)
            root ((pitch-scale tonic scale) (dec degree))]
        {:scale scale :tonic tonic :notes notes :root root
         :type (combine-keywords root (chord-type notes))
         :pitches (pitch-chord tonic scale shape degree)}))))

(chord-set :C :major)

(defn- normalize-dissonance
  "Within given scales, normalizes dissonance values from 0 to 1 for each chord."
  [scales]
  (let [filtered (select-keys scale-dissonance scales)
        dissonance-vals (clojure.core/flatten (vals filtered))
        min-dissonance (apply min dissonance-vals)
        max-dissonance (apply max dissonance-vals)
        diff (- max-dissonance min-dissonance)]
    (map-vals (fn [vals]
                (map #(-> % (- min-dissonance) (/ diff)) vals))
              filtered)))

(defn- find-scale
  "Finds a scale that corresponds to a target dissonance, and a tonic that
  corresponds to a target color."
  [scales prev-chord target-color target-dissonance]
  (let [scale (first (min-by #(abs-diff target-dissonance (avg (second %)))
                             (normalize-dissonance scales)))
        tonic (->> (scale-brightness scale)
                   (- (scale-brightness (:scale prev-chord)))
                   (+ (Math/round (double (* 5 target-color))))
                   (fifths-above (:tonic prev-chord)))]
    [tonic scale]))

(defn generate-progression
  "Generates a set of chords that matches tension curves within the given scales."
  ([tensions scales] (generate-progression tensions scales :C))
  ([{:keys [color dissonance gravity]} scales start-tonic]
   (reductions (fn [prev pos]
                 (let [[targ-col targ-dis targ-gra] (map #(% pos)
                                                         [color dissonance gravity])
                       [tonic scale] (find-scale scales prev targ-col targ-dis)
                       chords (chord-set tonic scale)
                       col-scores (map (fn [chord]
                                         (->> (:pitches chord)
                                              (chord-color (:pitches prev))
                                              (#(/ % 5)) (abs-diff targ-col)))
                                       chords)
                       dis-scores (map (partial abs-diff targ-dis)
                                       (scale (normalize-dissonance scales)))
                       gra-scores (map #(when-let [g (chord-gravity (:notes prev)
                                                                    (:notes %))]
                                          (max (- targ-gra g) 0))
                                       chords)]
                   (apply-scores [col-scores dis-scores gra-scores] chords)))
               (first (chord-set start-tonic (first scales)))
               (range 0 (count color)))))

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
