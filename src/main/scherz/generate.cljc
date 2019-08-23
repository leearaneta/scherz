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
  [scale tonic]
  (let [note-ct (count (u/scale-intervals scale))
        pitched-scale (b/pitch-scale tonic scale)]
    (for [shape (u/chord-shapes note-ct)
          degree (range 1 (inc note-ct))
          inversion (range (count shape))]
      (let [notes (u/base-chord tonic scale shape degree)
            root (pitched-scale (dec degree))
            pitches (b/pitch-chord tonic scale shape degree)
            type (u/chord-type notes)]
        {:scale scale :tonic tonic
         :notes (g/invert notes inversion)
         :pitches (u/rotate pitches inversion)
         :type (if (nil? type) "" (str root (name type)))}))))

(defn- filter-chords
  [scales prev-chord target-color]
  (mapcat (fn [scale]
         (->> (b/scale-brightness scale)
              (- (b/scale-brightness (:scale prev-chord)))
              (+ (Math/round (double (* 5 target-color))))
              (b/fifths-above (:tonic prev-chord))
              (chord-set scale)))
          scales))

(defn- normalize-dissonance
  "With a set of scales, returns a function that takes in a dissonance value and
   outputs a normalized dissonance value from 0 to 1."
  [scales]
  (let [dissonance-vals (->> scales
                             (mapcat #(chord-set % "C"))
                             (map :notes)
                             (map d/chord-dissonance))
        min-dissonance (apply min dissonance-vals)
        max-dissonance (apply max dissonance-vals)
        diff (- max-dissonance min-dissonance)]
    (fn [dissonance] (-> dissonance (- min-dissonance) (/ diff)))))

(defn- next-chord
  "With a set of scales, returns a function that finds an appropriate following
   chord from the previous chord and a set of tensions."
  [scales]
  (let [normalize-dissonance (normalize-dissonance scales)]
    (fn [prev tension]
      (let [tension #?(:clj tension
                       :cljs (js->clj tension :keywordize-keys :true))
            {:keys [color dissonance gravity]} tension
            chords (filter-chords scales prev color)
            score-color (fn [chord]
                          (->> (:pitches chord)
                               (b/chord-color (:pitches prev))
                               (#(/ % 5))
                               (u/abs-diff color)))
            color-scores (map score-color chords)
            score-dissonance (comp (partial u/abs-diff dissonance)
                                   normalize-dissonance
                                   d/chord-dissonance
                                   :notes)
            dissonance-scores (map score-dissonance chords)
            score-gravity (fn [chord]
                            (when-let [g (g/chord-gravity (:notes prev)
                                                          (:notes chord))]
                              (max (- gravity g) 0)))
            gravity-scores (map score-gravity chords)]
        (apply-scores [color-scores dissonance-scores gravity-scores]
                      chords)))))

(defn generate-progression
  "Generates a set of chords that matches tension curves within the given scales."
  ([tensions scales] (generate-progression tensions scales "C"))
  ([tensions scales start-tonic]
   (let [scales #?(:clj scales
                   :cljs (map keyword scales))]
     (reductions (next-chord scales)
                 (first (chord-set (first scales) start-tonic))
                 tensions))))

(defn voice-progression
  [progression]
  (let [voice-chord (fn [chord octave]
                      (->> (:notes chord)
                           g/compress
                           (map (partial + (* octave 12)))
                           (assoc chord :notes)))
        optimal-voicing (fn [prev chord]
                          (->> '(5 6) ; valid octaves
                               (map (partial voice-chord chord))
                               (map :notes)
                               (u/max-by (partial g/chord-gravity (:notes prev)))
                               (assoc chord :notes)))]
    (reductions optimal-voicing
                (voice-chord (first progression) 5)
                (rest progression))))

(defn clean-progression [progression]
  {:post [(= (map (comp count :pitches) %)
             (map (comp count :notes) %))]}
  (map (fn [chord]
         (->> (select-keys chord [:notes :pitches])
              (u/map-vals dedupe)
              (into chord)))
       progression))

(def main (comp voice-progression generate-progression))

(time (main [{:color 0 :dissonance 0 :gravity 0}
             {:color 0.4 :dissonance 0.6 :gravity 0}
             {:color 0 :dissonance 0.4 :gravity 0.4}
             {:color 0.5 :dissonance 0.25 :gravity 0}]
            [:lydian :melodic-minor :diminished]))
