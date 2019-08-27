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
        pitched-scale (b/pitch-scale tonic scale)]
    (for [shape (u/chord-shapes note-ct)
          degree (range 1 (inc note-ct))
          inversion (range (count shape))]
      (let [notes (u/base-chord tonic scale shape degree)
            root (pitched-scale (dec degree))
            pitches (b/pitch-chord tonic scale shape degree)
            type (u/chord-type notes)]
        {:scale scale :tonic tonic :inversion inversion :type type
         :notes (u/invert notes inversion)
         :pitches (u/rotate pitches inversion)
         :name (if (nil? type) "" (str root (name type)))}))))

(defn- filter-chords
  "Creates a set of chords to choose from according to target color."
  ; for some reason for comprehension doesn't flatten all the way
  [scales prev-chord target-color]
  (mapcat (fn [scale]
            (let [target-color (Math/round (double (* 5 target-color)))
                  fs (if (= target-color 0) [+] [+ -])]
              (mapcat (fn [f]
                        (-> (b/scale-brightness scale)
                            (f target-color)
                            (- (b/scale-brightness (:scale prev-chord)))
                            (b/fifths-above (:tonic prev-chord))
                            (chord-set scale)))
                      fs)))
          scales))

(defn- normalize-dissonance
  "With a set of scales, returns a function that takes in a dissonance value and
   outputs a normalized dissonance value from 0 to 1."
  [scales]
  (let [dissonance-vals (->> scales
                             (mapcat (partial chord-set "C"))
                             (map :notes)
                             (map d/chord-dissonance))
        min-dissonance (apply min dissonance-vals)
        max-dissonance (apply max dissonance-vals)
        diff (- max-dissonance min-dissonance)]
    (fn [dissonance] (-> dissonance (- min-dissonance) (/ diff)))))

(defn- generate-chord
  [scales prev {:keys [color dissonance gravity]}]
  (let [normalize-dissonance (normalize-dissonance scales)
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
                        (when-let [g (g/chord-gravity (g/compress (:notes prev))
                                                      (:notes chord))]
                          (max (- gravity g) 0)))
        gravity-scores (map score-gravity chords)]
    (apply-scores [color-scores dissonance-scores gravity-scores] chords)))

(defn transfer-chord
  "Moves notes in a chord by the given amount of octaves."
  [octave notes]
  (map (partial + (* octave 12))
       (g/compress notes)))

(defn- voice-chord
  "Given a previous chord, moves notes in a chord between octaves 5 and 6 depending
   on which voicing is closer."
  [prev chord]
  (->> '(5 6)
       (map (fn [octave] (transfer-chord octave (:notes chord))))
       (u/max-by (partial g/chord-gravity (:notes prev)))
       (assoc chord :notes)))

(defn- clean-chord
  "Dedupes pitches and notes in a chord."
  [chord]
  (->> (select-keys chord [:notes :pitches])
       (u/map-vals (fn [_ v] (dedupe v)))
       (into chord)))

(defn next-chord
  [scales prev tension]
  (let [scales #?(:clj scales :cljs (map keyword scales))
        prev #?(:clj prev :cljs (js->clj prev :keywordize-keys :true))
        tension #?(:clj tension :cljs (js->clj tension :keywordize-keys :true))]
    (->> (generate-chord scales prev tension)
         (voice-chord prev)
         clean-chord)))

(defn possible-types
  "Outputs all possible chord types given a set of scales."
  [scales]
  (->> scales
       (map keyword)
       (mapcat (fn [scale]
                 (let [note-ct (count (u/scale-intervals scale))]
                   (for [shape (u/chord-shapes note-ct)
                         degree (range 1 (inc note-ct))]
                     (u/chord-type (u/base-chord "C" scale shape degree))))))
       distinct
       (remove nil?)))

(defn initial-chord
  [scales root type]
  {:pre [(some #(= % (keyword type)) (possible-types scales))]}
  (let [chord (->> scales
                   (map keyword)
                   (mapcat (partial chord-set root))
                   (filter (fn [chord] (= (:type chord) (keyword type))))
                   first)]
    (->> (:notes chord) (transfer-chord 5) (assoc chord :notes))))

(defn generate-progression
  "Repeatedly calls next-chord to generate a chord progression."
  ([scales tensions]
   (generate-progression scales tensions "C"))
  ([scales tensions root]
   (generate-progression scales tensions root (first (possible-types scales))))
  ([scales tensions root type]
   (reductions (partial next-chord scales)
               (initial-chord scales root type)
               tensions)))

