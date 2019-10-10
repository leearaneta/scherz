(ns scherz.generate
  (:require #?(:clj [scherz.util :as u]
               :cljs [scherz.util :as u :include-macros true])
            [scherz.scale :as s]
            [scherz.gravity :as g]
            [scherz.brightness :as b]
            [scherz.chord :as c]
            [scherz.dissonance :as d]))

(defn- apply-scores
  "Picks a chord that has the lowest combined score."
  [chords & flist]
  (let [combine-scores (fn [& scores]
                         (reduce (u/fwhen [a b] (+ a b)) scores))
        cost (fn [[_ score]]
               (if (nil? score) u/infinity score))]
    (->> flist
         (map (fn [f] (map f chords)))
         (apply map combine-scores)
         (map vector chords)
         (u/min-by-coll cost)
         (map first))))

(defn- compress [notes]
  (if (some #(< % 12) notes)
    notes
    (recur (map #(- % 12) notes))))

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

(defn- chord-sets
  "Returns a list of chords that fall within the given scale and target color from
   the previous chord."
  [prev-chord target-color arc scale]
  (let [target-color (Math/round (double (* 5 target-color)))
        fs (cond
             (or (= target-color 0) (= arc :asc)) [+]
             (= arc :desc) [-]
             :else [+ -])]
    (mapcat (fn [f]
              (-> (b/scale-brightness scale)
                  (f target-color)
                  (- (b/scale-brightness (:scale prev-chord)))
                  (b/fifths-above (:tonic prev-chord))
                  (c/chord-set scale)))
            fs)))

(defn- valid-tension? [{:keys [color dissonance gravity]}]
  (every? #(<= 0 % 1) [color dissonance gravity]))

(defn- generate-chords
  [scales prev tension]
  {:pre [(every? s/valid-scale? scales)
         (valid-tension? tension)]}
  (let [scales #?(:clj scales :cljs (map keyword scales))
        prev #?(:clj prev :cljs (js->clj prev :keywordize-keys :true))
        tension #?(:clj tension :cljs (js->clj tension :keywordize-keys :true))
        {:keys [color dissonance gravity arc]} tension
        chords (mapcat (partial chord-sets prev color arc) scales)
        score-color (fn [chord]
                      (->> (:pitches chord)
                           (b/chord-color (:pitches prev))
                           (#(/ % 5))
                           (u/abs-diff color)))
        score-dissonance (comp (partial u/abs-diff dissonance)
                               (d/normalize-dissonance scales)
                               d/chord-dissonance
                               :notes)
        score-gravity (fn [chord]
                        (when-let [g (g/chord-gravity (compress (:notes prev))
                                                      (:notes chord))]
                          (max (- gravity g) 0)))]
    (map (partial voice-chord prev)
         (apply-scores chords score-color score-dissonance score-gravity))))

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

(defn- next-chord
  ([scales prev tension] (next-chord 0 scales prev tension))
  ([seed scales prev tension]
   (let [chords (generate-chords scales prev tension)]
     (get (vec chords)
          (mod seed (count chords))))))

(defn generate-progression
  "Repeatedly calls next-chord to generate a chord progression."
  [scales tensions {:keys [root type seed]
                    :or {root "C"
                         type (first (c/possible-chord-types scales))
                         seed 0}}]
  (reductions (partial next-chord (int seed) scales)
              (initial-chord scales root type)
              tensions))
