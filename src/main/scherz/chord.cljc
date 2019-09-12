(ns scherz.chord
  (:require [scherz.scale :refer [scale-intervals valid-scale?]]
            [scherz.brightness :refer [pitch-scale pitch-chord]]
            [scherz.gravity :refer [condense note-invert pitch-invert]]))

(defn pitch->midi [pitch]
  (let [notes {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11}
        multiplier (if (= \# (last pitch)) 1 -1)]
    (-> (dec (count pitch))
        (* multiplier)
        (+ (notes (first pitch)))
        (mod 12))))

(defn base-chord [tonic scale chord-shape degree]
  (->> (cycle (scale-intervals scale))
       (reductions + (pitch->midi tonic))
       (drop (dec degree))
       (take (inc (last chord-shape)))
       (#(map (vec %) chord-shape))))

(def chord-shapes
  {6 [[0 2 4 6] [0 2 4 5]]
   7 [[0 2 4 7] [0 2 4 6] [0 1 4 6] [0 3 4 6] [0 1 2 4]]
   8 [[0 2 4 6] [0 2 4 7] [0 2 4 8] [0 2 5 7] [0 2 5 8]]})

(def chord-types
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

(defn chord-type [notes]
  (first (filter (fn [k] (= (k chord-types)
                            (map #(- % (first notes)) notes)))
                 (keys chord-types))))

(defn chord-set
  "Returns all chords within a given tonic / scale."
  [tonic scale]
  (let [note-ct (count (scale-intervals scale))
        pitched-scale (pitch-scale tonic scale)]
    (for [shape (chord-shapes note-ct)
          degree (range 1 (inc note-ct))
          inversion (-> shape (condense note-ct) count range)]
      (let [notes (base-chord tonic scale shape degree)
            root (pitched-scale (dec degree))
            pitches (pitch-chord tonic scale shape degree)
            type (chord-type notes)]
        {:scale scale :tonic tonic :inversion inversion :type type
         :notes (note-invert notes inversion)
         :pitches (pitch-invert pitches inversion)
         :name (if (nil? type) "" (str root (name type)))}))))

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
                     (chord-type (base-chord "C" scale shape degree))))))
       distinct
       (remove nil?)))

(defn possible-chord-type? [scales type]
  (some #(= % (keyword type)) (possible-chord-types scales)))
