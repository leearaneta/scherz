(ns scherz.generate
  (:require #?(:clj [scherz.util :as u]
               :cljs [scherz.util :as u :include-macros true])
            [scherz.dissonance :as d]
            [scherz.gravity :as g]
            [scherz.brightness :as b]))

(defn- apply-scores
  "Picks a chord from values that has the lowest combined score."
  [scores values seed]
  (let [combine-scores (fn [& args] (reduce (u/fwhen [a b] (+ a b)) args))
        cost (fn [[_ score]]
               (if (nil? score) u/infinity score))]
    (->> scores
         (apply map combine-scores)
         (map vector values)
         (u/min-by seed cost)
         first)))

(defn chord-set
  "Returns all chords within a given tonic / scale."
  [tonic scale]
  (let [note-ct (count (u/scale-intervals scale))
        pitched-scale (b/pitch-scale tonic scale)]
    (for [shape (u/chord-shapes note-ct)
          degree (range 1 (inc note-ct))
          inversion (-> shape (u/condense note-ct) count range)]
      (let [notes (u/base-chord tonic scale shape degree)
            root (pitched-scale (dec degree))
            pitches (b/pitch-chord tonic scale shape degree)
            type (u/chord-type notes)]
        {:scale scale :tonic tonic :inversion inversion :type type
         :notes (u/note-invert notes inversion)
         :pitches (u/pitch-invert pitches inversion)
         :name (if (nil? type) "" (str root (name type)))}))))

(defn- filter-chords
  "Creates a set of chords to choose from according to target color."
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

(defn- valid-tension? [{:keys [color dissonance gravity]}]
  (every? #(<= 0 % 1) [color dissonance gravity]))

(defn compress
  [notes]
  (if (some #(< % 12) notes)
    notes
    (recur (map #(- % 12) notes))))

(defn- generate-chord
  [scales prev {:keys [color dissonance gravity] :as tension} seed]
  {:pre [(every? u/valid-scale? scales)
         (valid-tension? tension)]}
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
                        (when-let [g (g/chord-gravity (compress (:notes prev))
                                                      (:notes chord))]
                          (max (- gravity g) 0)))
        gravity-scores (map score-gravity chords)]
    (apply-scores [color-scores dissonance-scores gravity-scores] chords seed)))

(defn transfer-chord
  "Moves notes in a chord by the given amount of octaves."
  [octave notes]
  (map (partial + (* octave 12))
       (compress notes)))

(defn- voice-chord
  "Given a previous chord, moves notes in a chord between 58 and 82 depending
   on which voicing is closer."
  [prev chord]
  (->> '(4 5 6)
       (map (fn [octave] (transfer-chord octave (:notes chord))))
       (filter (fn [notes] (every? #(<= 58 % 82) notes)))
       (u/max-by (partial g/chord-gravity (:notes prev)))
       (assoc chord :notes)))

(defn- clean-chord
  "Dedupes pitches and notes in a chord."
  [chord]
  (->> (select-keys chord [:notes :pitches])
       (u/map-vals (fn [_ v] (dedupe v)))
       (into chord)))

(defn next-chord
  ([scales prev tension] (next-chord 0 scales prev tension))
  ([seed scales prev tension]
   (let [scales #?(:clj scales :cljs (map keyword scales))
         prev #?(:clj prev :cljs (js->clj prev :keywordize-keys :true))
         tension #?(:clj tension :cljs (js->clj tension :keywordize-keys :true))]
     (->> (generate-chord scales prev tension seed)
          (voice-chord prev)
;          clean-chord
          ))))

(defn initial-chord
  [scales root type]
  {:pre [(every? u/valid-scale? scales)
         (b/valid-pitch? root)
         (u/possible-type? scales type)]}
  (let [chord (->> scales
                   (map keyword)
                   (mapcat (partial chord-set root))
                   (filter (fn [chord] (= (:type chord) (keyword type))))
                   first)]
    (->> (:notes chord) (transfer-chord 5) (assoc chord :notes))))

(defn generate-progression
  "Repeatedly calls next-chord to generate a chord progression."
  [scales tensions {:keys [root type seed]
                    :or {root "C" type (first (u/possible-types scales)) seed 0}}]
  (reductions (partial next-chord (int seed) scales)
              (initial-chord scales root type)
              tensions))
