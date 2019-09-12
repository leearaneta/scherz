(ns scherz.generate
  (:require #?(:clj [scherz.util :as u]
               :cljs [scherz.util :as u :include-macros true])
            [scherz.scale :as s]
            [scherz.gravity :as g]
            [scherz.brightness :as b]
            [scherz.chord :as c]
            [scherz.dissonance :as d]))

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
                            (c/chord-set scale)))
                      fs)))
          scales))

(defn- valid-tension? [{:keys [color dissonance gravity]}]
  (every? #(<= 0 % 1) [color dissonance gravity]))

(defn- compress [notes]
  (if (some #(< % 12) notes)
    notes
    (recur (map #(- % 12) notes))))

(defn- generate-chord
  [scales prev {:keys [color dissonance gravity] :as tension} seed]
  {:pre [(every? s/valid-scale? scales)
         (valid-tension? tension)]}
  (let [normalize-dissonance (d/normalize-dissonance scales)
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
  {:pre [(every? s/valid-scale? scales)
         (b/valid-pitch? root)
         (c/possible-chord-type? scales type)]}
  (let [chord (->> scales
                   (map keyword)
                   (mapcat (partial c/chord-set root))
                   (filter (fn [chord] (= (:type chord) (keyword type))))
                   first)]
    (->> (:notes chord) (transfer-chord 5) (assoc chord :notes))))

(defn generate-progression
  "Repeatedly calls next-chord to generate a chord progression."
  [scales tensions {:keys [root type seed]
                    :or {root "C"
                         type (first (c/possible-chord-types scales))
                         seed 0}}]
  (reductions (partial next-chord (int seed) scales)
              (initial-chord scales root type)
              tensions))
