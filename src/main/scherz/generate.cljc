(ns scherz.generate
  (:require #?(:clj [scherz.util :as u]
               :cljs [scherz.util :as u :include-macros true])
            [scherz.dissonance :as d]
            [scherz.gravity :as g]
            [scherz.brightness :as b]))

(defn- apply-scores
  "Picks a chord from values that has the lowest combined score."
  [scores values]
  (let [combine-scores (fn [& args] (reduce (u/fwhen [a b] (+ a b)) args))
        cost (fn [[_ score]]
               (if (nil? score) u/infinity score))]
    (->> scores
         (apply map combine-scores)
         (map vector values)
         (u/min-by cost)
         first)))

(defn chord-set
  "Returns all chords within a given tonic / scale."
  [tonic scale]
  (let [note-ct (count (u/scale-intervals scale))
        combine-keywords (u/fwhen [& keywords]
                           (->> keywords (map name) (apply str) keyword))
        pitched-scale (b/pitch-scale tonic scale)]
    (for [shape (u/chord-shapes note-ct)
          degree (range 1 (inc note-ct))]
      (let [notes (u/base-chord tonic scale shape degree)
            root (pitched-scale (dec degree))
            pitched-chord (b/pitch-chord tonic scale shape degree)]
        {:scale scale :tonic tonic :notes notes :root root
         :pitches pitched-chord
         :type (combine-keywords root (u/chord-type notes))}))))

(defn- normalize-dissonance
  "Within given scales, normalizes dissonance values from 0 to 1 for each chord."
  [scales]
  (let [filtered (select-keys d/scale-dissonance scales)
        dissonance-vals (clojure.core/flatten (vals filtered))
        min-dissonance (apply min dissonance-vals)
        max-dissonance (apply max dissonance-vals)
        diff (- max-dissonance min-dissonance)]
    (u/map-vals (fn [vals]
                  (map #(-> % (- min-dissonance) (/ diff)) vals))
                filtered)))

(defn- find-scale
  "Finds a scale that corresponds to a target dissonance, and a tonic that
  corresponds to a target color."
  [scales prev-chord target-color target-dissonance]
  (let [scale (first (u/min-by #(u/abs-diff target-dissonance (u/avg (second %)))
                               (normalize-dissonance scales)))
        tonic (->> (b/scale-brightness scale)
                   (- (b/scale-brightness (:scale prev-chord)))
                   (+ (Math/round (double (* 5 target-color))))
                   (b/fifths-above (:tonic prev-chord)))]
    [tonic scale]))

(defn generate-progression
  "Generates a set of chords that matches tension curves within the given scales."
  ([tensions scales] (generate-progression tensions scales :C))
  ([tensions scales start-tonic]
   (reductions (fn [prev tension]
                 (let [tension #?(:clj tension
                                  :cljs (js->clj tension :keywordize-keys :true))
                       {:keys [color dissonance gravity]} tension
                       [tonic scale] (find-scale scales prev color dissonance)
                       chords (chord-set tonic scale)
                       score-color (fn [chord]
                                     (->> (:pitches chord)
                                          (b/chord-color (:pitches prev))
                                          (#(/ % 5))
                                          (u/abs-diff color)))
                       color-scores (map score-color chords)
                       score-dissonance (partial u/abs-diff dissonance)
                       dissonance-scores (map score-dissonance
                                              (scale (normalize-dissonance scales)))
                       score-gravity (fn [chord]
                                       (when-let [g (g/chord-gravity (:notes prev)
                                                                     (:notes chord))]
                                         (max (- gravity g) 0)))
                       gravity-scores (map score-gravity chords)]
                   (apply-scores [color-scores dissonance-scores gravity-scores]
                                 chords)))
               (first (chord-set start-tonic (first scales)))
               tensions)))

(defn voice-progression
  "Implement proper voice leading in a progression.
  If any note is below midi 60 or above midi 80, invert the chord."
  [progression]
  (let [constrain-voicing (fn [notes]
                            (cond (< (apply min notes) 60) (g/invert notes 1)
                                  (< 80 (apply max notes)) (g/invert notes -1)
                                  :else notes))
        voice-notes (fn [prev-chord chord]
                      (->> (:notes prev-chord)
                           (#(g/voice-lead % (:notes chord)))
                           constrain-voicing
                           (assoc chord :notes)))
        voice-pitches (fn [chord]
                        (->> (:notes chord)
                             (g/inversion (u/pitch->midi (:root chord)))
                             (u/rotate (:pitches chord))
                             (assoc chord :pitches)))
        initial-chord (->> (:notes (first progression))
                           (map (partial + 60))
                           constrain-voicing
                           (assoc (first progression) :notes)
                           voice-pitches)]
    (reductions (comp voice-pitches voice-notes)
                initial-chord
                (rest progression))))

(defn clean-progression [progression]
  {:post [(= (map (comp count :pitches) %)
             (map (comp count :notes) %))]}
  (let [dedupe-chord (fn [chord]
                       (->> (select-keys chord [:notes :pitches])
                            (u/map-vals distinct)
                            (into chord)))
        add-pitch (fn [{:keys [pitches notes] :as chord}]
                    (if (not= (count pitches) (count notes))
                      (assoc chord :pitches (conj (vec pitches)
                                                  (first pitches)))
                      chord))]
    (->> progression
         (map dedupe-chord)
         (map add-pitch))))

(def main (comp clean-progression voice-progression generate-progression))
