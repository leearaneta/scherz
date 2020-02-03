(ns scherz.chord
  (:require [clojure.set :refer [difference]]
            [scherz.util :refer [find-coll insert abs-diff min-by distinct-by]]
            [scherz.scale :refer [scales scale-intervals valid-scale?]]
            [scherz.brightness :refer [pitch->brightness pitch-scale
                                       pitch-chord fifths-above]]
            [scherz.gravity :refer [condense sink-octave open-voicing
                                    note-invert pitch-invert transfer-octaves]]
            [scherz.dissonance :refer [chord-dissonance]]))

(def chord-shapes
  "Mapping of scale lengths -> chord shapes (used to generate sets of chords).
  Each number in a chord shape represents the nth note in a scale."
  {6 [[0 2 4] [0 2 4 5]]
   7 [[0 2 4] [0 2 4 6]
      [0 1 4] [0 1 4 6]
      [0 3 4] [0 3 4 6]]
   8 [[0 2 4] [0 2 4 6] [0 2 4 7] [0 2 5] [0 2 5 7]]})

(defn pitch->midi [pitch]
  (let [notes {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11}
        multiplier (if (= \# (last pitch)) 1 -1)]
    (-> (dec (count pitch))       ; find the amount of sharps or flats
        (* multiplier)            ; multiply by 1 or -1
        (+ (notes (first pitch))) ; add to base value
        (mod 12))))

(defn base-notes
  "(base-notes :major [0 2 4 6] 2) -> [2 5 9 12]
   Returns midi notes from the given scale / shape at the nth scale degree."
  ([scale chord-shape degree] (base-notes "C" scale chord-shape degree))
  ([tonic scale chord-shape degree]
   (->> (cycle (scale-intervals scale)) ; infinite seq of scale intervals
        (reductions + (pitch->midi tonic)) ; seq of midi notes in scale (ascending)
        (drop (dec degree)) ; start infinite seq at corresponding scale degree
        (take (inc (last chord-shape))) ; trim scale
        (#(mapv (vec %) chord-shape))
        (mapv (partial + 12)))))

(def chord-types
  "Mapping of chord types to midi notes (starting at 0)."
  {:M      [0 4 7]
   :m      [0 3 7]
   :°      [0 3 6]
   :+      [0 4 8]
   :M7     [0 4 7 11]
   :D7     [0 4 7 10]
   :m7     [0 3 7 10]
   :°7     [0 3 6 9]
   :ø7     [0 3 6 10]
   :+7     [0 4 8 10]
   :+M7    [0 4 8 11]
   :mM7    [0 3 7 11]
   :°M7    [0 3 6 11]
   :7sus2  [0 2 7 10]
   :7sus4  [0 5 7 10]
   :M7sus2 [0 2 7 11]
   :M7sus4 [0 5 7 11]})

(defn chord-type
  "From a set of midi notes, finds the corresponding chord type."
  [notes]
  (find-coll (fn [type] (= (chord-types type)
                           (map #(- % (first notes)) notes)))
             (keys chord-types)))

(defn- add-extension
  [{:keys [notes pitches tonic scale root] :as chord} [pitch note]]
  (let [new-notes (vec (sort (conj notes note)))
        new-pitches (insert pitches (.indexOf new-notes note) pitch)
        pitched-scale (pitch-scale tonic scale)
        degree (inc (.indexOf (drop-while (partial not= root)
                                          (cycle pitched-scale))
                              pitch))
        extension (cond
                    (= root pitch) nil
                    (< note (first notes)) {:bass pitch}
                    (< note (+ 12 (first notes))) {:degree degree}
                    :else {:degree (+ degree (count pitched-scale))})]
    (-> chord
        (assoc :notes new-notes)
        (assoc :pitches new-pitches)
        (into extension))))

(defn add-extensions [{:keys [scale tonic pitches notes root] :as chord}]
  (let [extensions (fn [pitch]
                     (->> (pitch->midi pitch)
                          (iterate (partial + 12))
                          (drop-while (partial >= (- (first notes) 12)))
                          (take-while (partial > (last notes)))
                          (remove (partial contains? (set notes)))
                          (map (partial vector pitch))))
        excluded-pitches (if (= (count notes) 3)
                           (disj (set pitches) root)
                           (set pitches))]
    (->> excluded-pitches
         (difference (set (pitch-scale tonic scale)))
         (mapcat extensions)
         (map (partial add-extension chord)))))

(defn- chord-priority [chords]
  (let [cost (fn [{:keys [bass degree type inversion]}]
               (cond
                 (some? degree) 1
                 (some? bass) 2
                 (not= inversion 0) 3
                 (nil? type) 4
                 :else 0))]
    (min-by cost chords)))

(defn- any-clustered-notes? [notes]
  (let [consecutive? (fn [pairs]
                       (->> pairs
                            (map (partial apply abs-diff))
                            (every? (partial > 3))))]
    (->> (partition 3 1 notes)
         (map (partial partition 2 1))
         (some consecutive?))))

(defn- any-sevenths? [notes]
  (->> (pop (apply list notes))
       (partition 2 1)
       (map (partial apply abs-diff))
       (some (partial <= 10))))

(defn- negligible-bass? [notes]
  (and (<= 3 (- (second notes) (first notes)) 4)
       (or (< 6 (- (nth notes 2) (second notes)))
           (< 8 (- (nth notes 3) (nth notes 2))))))

(defn- any-muddy-intervals? [notes]
  (let [muddy? (fn [[from to]]
                 (cond
                   (< from 50) (< (- to from) 7)
                   (< from 55) (< (- to from) 5)
                   (< from 59) (< (- to from) 4)
                   :else false))]
    (some muddy? (partition 2 1 notes))))

(defn- base-chord-set
  "Returns chords within a given scale in C."
  [scale]
  (let [note-ct (count (scale-intervals scale))
        pitched-scale (vec (pitch-scale "C" scale))
        chords (for [shape (chord-shapes note-ct)
                     degree (range 1 (inc note-ct))
                     inversion (-> shape (condense note-ct) count range)]
                 (let [notes (base-notes "C" scale shape degree)
                       root (pitched-scale (dec degree))
                       pitches (pitch-chord "C" scale shape degree)
                       type (when-let [t (chord-type notes)] (name t))]
                   {:scale scale :tonic "C" :root root
                    :type type :inversion inversion
                    :pitches (pitch-invert pitches inversion)
                    :notes (note-invert notes inversion)}))]
    (->> chords
         (mapcat (fn [chord] [chord (open-voicing chord)]))
         (mapcat (fn [chord] (conj (add-extensions chord) chord)))
         (distinct-by (comp sink-octave :notes) chord-priority)
         (remove (fn [chord] (= 3 (count (:notes chord)))))
         (remove (comp any-clustered-notes? :notes))
         (remove (comp any-sevenths? :notes))
         (remove (comp negligible-bass? :notes))
         (map (fn [chord]
                (assoc chord :dissonance (chord-dissonance (:notes chord))))))))

(def base-chord-sets
  "Hashmap of base chord sets for all scales.
   {:major [ ... ] :minor [ ... ]}"
  (->> (map base-chord-set scales)
       (map vector scales)
       (into {})))

(defn chord-set
  "Finds all chords within the given scale in C, and then transposes each chord to
   fall within the given tonic."
  [tonic scale]
  (let [brightness (dec (pitch->brightness tonic))
        note (min-by (partial abs-diff 0)
                     [(pitch->midi tonic) (- (pitch->midi tonic) 12)])]
    (->> (scale base-chord-sets)
         (map (fn [{:keys [root notes type pitches bass degree dissonance]}]
                {:tonic tonic :scale scale :dissonance dissonance
                 :notes (map (partial + note) notes)
                 :pitches (map (partial fifths-above brightness) pitches)
                 :type (when (and (some? type) (nil? bass))
                         (str (name type) (when degree (str "add" degree))))
                 :name (when type
                         (-> (fifths-above brightness root)
                             (str (name type))
                             (str (when bass
                                    (str "/" (fifths-above brightness bass))))
                             (str (when degree
                                    (str "add" degree)))))}))
         (mapcat transfer-octaves)
         (remove (comp any-muddy-intervals? :notes)))))

(defn possible-chord-types
  "Outputs all possible chord types given a set of scales."
  [scales]
  {:pre [(every? valid-scale? scales)]}
  (->> (map keyword scales)
       (mapcat (fn [scale]
                 (let [note-ct (count (scale-intervals scale))]
                   (for [shape (chord-shapes note-ct)
                         degree (range 1 (inc note-ct))]
                     (chord-type (base-notes scale shape degree))))))
       distinct
       (remove nil?)))

(defn possible-chord-type? [scales type]
  (some (partial = (keyword type))
        (possible-chord-types scales)))
