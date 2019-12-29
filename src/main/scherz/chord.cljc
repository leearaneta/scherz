(ns scherz.chord
  (:require [scherz.util :refer [find-coll]]
            [scherz.scale :refer [scales scale-intervals valid-scale?]]
            [scherz.brightness :refer [pitch->brightness pitch-scale
                                       pitch-chord fifths-above]]
            [scherz.gravity :refer [condense note-invert pitch-invert]]))

(def chord-shapes
  "Mapping of scale lengths -> chord shapes (used to generate sets of chords).
  Each number in a chord shape represents the nth note in a scale."
  {6 [[0 2 4 6] [0 2 4 5]]
   7 [[0 2 4 7] [0 2 4 6] [0 1 4 6] [0 3 4 6] [0 1 2 4]]
   8 [[0 2 4 6] [0 2 4 7] [0 2 4 8] [0 2 5 7] [0 2 5 8]]})

(defn pitch->midi [pitch]
  (let [notes {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11}
        multiplier (if (= \# (last pitch)) 1 -1)]
    (-> (dec (count pitch))       ; find the amount of sharps or flats
        (* multiplier)            ; multiply by 1 or -1
        (+ (notes (first pitch))) ; add to base value
        (mod 12))))

(defn base-notes
  "(base-notes :major [0 2 4 6] 2) -> (2 5 9 12)
   Returns midi notes from the given scale / shape at the nth scale degree."
  ([scale chord-shape degree] (base-notes "C" scale chord-shape degree))
  ([tonic scale chord-shape degree]
   (->> (cycle (scale-intervals scale)) ; infinite seq of scale intervals
        (reductions + (pitch->midi tonic)) ; seq of midi notes in scale (ascending)
        (drop (dec degree)) ; start infinite seq at corresponding scale degree
        (take (inc (last chord-shape)))  ; truncate sequence
        (#(map (vec %) chord-shape))))) ; index sequence according to chord shape

(def chord-types
  "Mapping of chord types to midi notes (starting at 0)."
  {:M      [0 4 7 12]
   :m      [0 3 7 12]
   :°      [0 3 6 12]
   :+      [0 4 8 12]
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
   :M7sus4 [0 5 7 11]
   :Madd2  [0 2 4 7]
   :madd2  [0 2 3 7]})

(defn chord-type
  "From a set of midi notes, finds the corresponding chord type."
  [notes]
  (find-coll (fn [type] (= (chord-types type)
                        (map #(- % (first notes)) notes)))
             (keys chord-types)))

(defn- base-chord-set
  "Returns all chords within a given scale in C."
  [scale]
  (let [note-ct (count (scale-intervals scale))
        pitched-scale (vec (pitch-scale "C" scale))]
    (for [shape (chord-shapes note-ct)
          degree (range 1 (inc note-ct))
          inversion (-> shape (condense note-ct) count range)]
      (let [notes (base-notes "C" scale shape degree)
            root (pitched-scale (dec degree))
            pitches (pitch-chord "C" scale shape degree)
            type (chord-type notes)]
        {:scale scale :tonic "C" :inversion inversion :root root :type type
         :notes (note-invert notes inversion)
         :pitches (pitch-invert pitches inversion)}))))

(def base-chord-sets
  "Hashmap of base chord sets for all scales.
   {:major [ ... ] :minor [ ... ]}"
  (->> scales
       (map base-chord-set)
       (map vector scales)
       (into {})))

(defn chord-set
  "Finds all chords within the given scale in C, and then transposes each chord to
   fall within the given tonic."
  [tonic scale]
  ; decrement brightness since base chords are in C, which has a brightness of 1
  (let [brightness (dec (pitch->brightness tonic))
        note (pitch->midi tonic)]
    (map (fn [{:keys [root type notes pitches] :as base-chord}]
           (conj base-chord
                 {:tonic tonic
                  :notes (map (partial + note) notes)
                  :pitches (map (partial fifths-above brightness) pitches)
                  :name (if (nil? type) ""
                            (str (fifths-above brightness root) (name type)))}))
         (scale base-chord-sets))))

(defn possible-chord-types
  "Outputs all possible chord types given a set of scales."
  [scales]
  {:pre [(every? valid-scale? scales)]}
  (->> scales
       (map keyword)
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
