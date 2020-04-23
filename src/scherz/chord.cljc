(ns scherz.chord
  (:require [clojure.core.reducers :as r]
            #?(:clj [clojure.spec.alpha :as spec]
               :cljs [cljs.spec.alpha :as spec]) 
            [clojure.set :refer [difference]]
            [clojure.string :refer [replace]]
            [clojure.math.combinatorics :refer [combinations]]
            [scherz.util :refer [find-coll insert abs-diff
                                 min-by distinct-by extent]]
            [scherz.scale :refer [scales scale-intervals]]
            [scherz.brightness :refer [pitch->brightness fifths-above
                                       pitch-chord pitch-scale temper]]
            [scherz.gravity :refer [condense sink-octave note-invert pitch-invert
                                    apply-voicings transfer-octaves note-distance]]
            [scherz.dissonance :refer [dissonance]]))

(def chord-shapes
  "Mapping of scale lengths -> chord shapes (used to generate sets of chords).
  Each number in a chord shape represents the nth note in a scale."
  {6 [[0 2 4] [0 2 4 5]]
   7 [[0 2 4] [0 2 4 6]
      [0 1 4] [0 1 4 6]
      [0 3 4] [0 3 4 6]]
   8 [[0 2 4] [0 2 4 6] [0 2 4 7] [0 2 5] [0 2 5 7]]})

(defn- pitch->midi [pitch]
  (let [notes {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11}
        multiplier (if (= \# (last pitch)) 1 -1)]
    (-> (dec (count pitch))       ; find the amount of sharps or flats
        (* multiplier)            ; multiply by 1 or -1
        (+ (notes (first pitch))) ; add to base value
        (mod 12))))

(defn- base-notes
  "(base-notes :major [0 2 4 6] 2) -> [2 5 9 12]
   Returns midi notes from the given scale / shape at the nth scale degree."
  ([scale chord-shape degree] (base-notes "C" scale chord-shape degree))
  ([tonic scale chord-shape degree]
   (->> (cycle (scale-intervals scale))
        (reductions + (pitch->midi tonic)) ; seq of midi notes in scale (ascending)
        (drop (dec degree))
        (take (inc (last chord-shape)))
        (#(mapv (vec %) chord-shape)))))

(def chord-types
  "Mapping of chord types to midi notes (starting at 0)."
  {:M        [0 4 7]
   :m        [0 3 7]
   :°        [0 3 6]
   :+        [0 4 8]
   :sus2     [0 2 7]
   :susb2    [0 1 7]
   :sus4     [0 5 7]
   :sus#4    [0 6 7]
   :sus4b5   [0 5 6]
   :M7       [0 4 7 11]
   :7        [0 4 7 10]
   :m7       [0 3 7 10]
   :°7       [0 3 6 9]
   :ø7       [0 3 6 10]
   :+7       [0 4 8 10]
   :+M7      [0 4 8 11]
   :mM7      [0 3 7 11]
   :°M7      [0 3 6 11]
   :7sus2    [0 2 7 10]
   :7sus4    [0 5 7 10]
   :7sus4b5  [0 5 6 10]
   :M7sus2   [0 2 7 11]
   :M7sus4   [0 5 7 11]
   :M7sus#4  [0 6 7 11]
   :M7sus4b5 [0 5 6 11]})

(def triadic-types
  #{:M :m :° :+ :M7 :7 :m7 :°7 :ø7 :+7 :+M7 :mM7 :°M7})

(defn chord-type
  "From a set of midi notes, finds the corresponding chord type."
  [notes]
  (find-coll (fn [type] (= (chord-types type)
                           (map #(- % (first notes)) notes)))
             (keys chord-types)))

(def extensions
  {1 "b2" 2 "2"
   3 "3" 4 "3"
   5 "4"
   6 "b5" 7 "5"
   8 "6" 9 "6"
   10 "b7" 11 "7"
   13 "b9" 14 "9"
   15 "10" 16 "10"
   17 "11"
   18 "b12" 19 "12"
   20 "13" 21 "13"
   22 "b7" 23 "7"})

(defn- add-extension
  "Given a base chord and an extension to add, appends :degree or :bass (or none)
   to a chord. :degree and :bass are later used to help give the chord a name."
  [{:keys [notes pitches root] :as chord} [pitch note]]
  (let [new-notes (vec (sort (conj notes note)))
        new-pitches (insert pitches (.indexOf new-notes note) pitch)
        interval (note-distance (pitch->midi pitch) (pitch->midi root))
        extension (cond
                    (= root pitch) nil
                    (< note (first notes)) {:bass pitch}
                    (< note (+ 12 (first notes))) {:degree (extensions interval)}
                    :else {:degree (extensions (+ 12 interval))})]
    (-> chord
        (assoc :notes new-notes)
        (assoc :pitches new-pitches)
        (into extension))))

(defn add-extensions
  [{:keys [scale tonic pitches notes root] :as chord}]
  (let [extensions (fn [pitch]
                     (->> (- (pitch->midi pitch) 12)
                          (iterate (partial + 12))
                          (drop-while (partial >= (- (first notes) 12)))
                          (take-while (partial > (+ (last notes) 12)))
                          (remove (partial contains? (set notes)))
                          (map (partial vector pitch))))
        excluded-pitches (if (= (count notes) 3)
                           (disj (set pitches) root)
                           (set pitches))]
    (->> excluded-pitches
         (difference (set (pitch-scale tonic scale)))
         (mapcat extensions)
         (map (partial add-extension chord)))))

(defn- chord-priority
  "Determines which chord is chosen in the event that there are multiple chords
   with the same notes in a chord set."
  [chords]
  (let [add (fn [initial pred val]
              (if pred (+ initial val) initial))
        cost (fn [{:keys [bass degree type inversion]}]
               (-> 0
                   (add (nil? type) 100)
                   (add (not (triadic-types (keyword type))) 8)
                   (add (some? bass) 6)
                   (add (not= inversion 0) 5)
                   (add (some? degree) 4)))]
    (min-by cost chords)))

(defn- any-clustered-notes?
  "Returns true if any three consecutive notes are a whole step or less away."
  [notes]
  (let [clustered? (fn [pairs]
                     (->> pairs
                          (map (partial apply abs-diff))
                          (every? (partial > 3))))]
    (->> (partition 3 1 notes)
         (map (partial partition 2 1))
         (some clustered?))))

(defn- any-sevenths?
  "Returns true if any consecutive notes (excluding the bass) are more
   than a seventh away."
  [notes]
  (->> (pop (apply list notes))
       (partition 2 1)
       (map (partial apply abs-diff))
       (some (partial <= 10))))

(defn- any-minor-ninths?
  "Returns true if any notes (excluding the bass) are a minor ninth away."
  [notes]
  (->> (combinations (pop (apply list notes)) 2)
       (map (partial apply abs-diff))
       (some (partial = 13))))

(defn- negligible-bass? [notes]
  (and (<= 3 (- (second notes) (first notes)) 4)
       (or (< 6 (- (nth notes 2) (second notes)))
           (< 8 (- (nth notes 3) (nth notes 2))))))

(defn- any-muddy-intervals?
  "Returns true if any intervals are too close to each other in a low register."
  [notes]
  (let [muddy? (fn [[from to]]
                 (cond
                   (< from 50) (< (- to from) 7)
                   (< from 55) (< (- to from) 5)
                   (< from 59) (< (- to from) 4)
                   :else false))]
    (some muddy? (partition 2 1 notes))))

(defn- any-overlapping-notes?
  "Returns true if any notes will be rendered on the same line in a staff."
  [{:keys [pitches notes]}]
  (let [overlapping? (fn [[[pitch1 pitch2] [note1 note2]]]
                       (and (= (first pitch1) (first pitch2))
                            (<= (abs-diff note1 note2) 1)))]
    (->> (partition 2 1 notes)
         (map vector (partition 2 1 pitches))
         (some overlapping?))))


(defn- base-chords
  "Iterates over every shape, degree, and inversion in a scale to generate chords."
  [scale]
  (let [note-ct (count (scale-intervals scale))
        pitched-scale (vec (pitch-scale "C" scale))]
    (for [shape (chord-shapes note-ct)
          degree (range 1 (inc note-ct))
          inversion (-> shape (condense note-ct) count range)]
      (let [notes (base-notes "C" scale shape degree)
            root (pitched-scale (dec degree))
            pitches (pitch-chord "C" scale shape degree)
            type (when-let [t (chord-type notes)] (name t))]
        {:scale scale :tonic "C" :root root
         :type type :inversion inversion
         :pitches (pitch-invert pitches inversion)
         :notes (note-invert notes inversion)}))))

(defn- scale->c-chords
  "Returns chords within a given scale in C."
  [scale]
  (->> (base-chords scale)
       (mapcat apply-voicings)
       (mapcat (fn [chord] (conj (add-extensions chord) chord)))
       (distinct-by (comp sink-octave :notes) chord-priority)
       (remove (comp (partial = 3) count :notes))
       (remove (comp any-clustered-notes? :notes))
       (remove (comp any-sevenths? :notes))
       (remove (comp any-minor-ninths? :notes))
       (remove (comp negligible-bass? :notes))
       (remove any-overlapping-notes?)
       (map (fn [{:keys [notes pitches] :as chord}]
              (assoc chord
                     :dissonance (dissonance notes)
                     :temper (temper pitches)
                     :cof-extent (extent (map pitch->brightness pitches)))))))

(def c-chords
  "Hashmap of base chord sets for all scales.
   {:major [ ... ] :minor [ ... ]}"
  (->> (map scale->c-chords scales)
       (map vector scales)
       (into {})))

(defn- transpose
  "Returns a function that transposes a chord in C to the given tonic."
  [tonic]
  (let [brightness (dec (pitch->brightness tonic))
        note (min-by (partial abs-diff 0)
                     [(pitch->midi tonic) (- (pitch->midi tonic) 12)])]
    (fn [{:keys [root notes type pitches bass degree cof-extent] :as chord}]
      (-> chord
          (assoc :tonic tonic
                 :notes (map (partial + note) notes)
                 :pitches (map (partial fifths-above brightness) pitches)
                 :cof-extent (map (partial + brightness) cof-extent)
                 :type (when (and (some? type) (nil? bass))
                         (str (name type) (when degree (str "add" degree))))
                 :name (when type
                         (-> (fifths-above brightness root)
                             (str (name type))
                             (str (when bass
                                    (str "/" (fifths-above brightness bass))))
                             (str (when degree
                                    (str "add" degree)))
                             (replace "#" "♯")
                             (replace "b" "♭"))))
          (dissoc :root :bass :degree)))))

(defn chord-set
  "Finds all chords within the given scale in C, and then transposes each chord to
   fall within the given tonic."
  [tonic scale]
  (->> (scale c-chords)
       (r/map (transpose tonic))
       (r/map (fn [chord] (assoc chord :scale scale)))
       (r/mapcat transfer-octaves)
       (r/remove (comp any-muddy-intervals? :notes))
       r/foldcat))

(spec/def ::notes (spec/* int?))
(spec/def ::chord (spec/keys :req-un [::notes
                                      :scherz.brightness/pitches
                                      :scherz.brightness/tonic
                                      :scherz.scale/scale]))
