(ns scherz.generate
  (:require [scherz.util])
  (:require [scherz.consonance])
  (:require [scherz.gravity])
  (:require [scherz.brightness]))

(refer 'scherz.util)
(refer 'scherz.consonance)
(refer 'scherz.gravity)
(refer 'scherz.brightness)

(defn- normalize [coll]
  (let [cleaned (filter some? coll)
        max-val (apply max cleaned)
        min-val (apply min cleaned)]
    (if (= max-val min-val)
      coll
      (map (fwhen [v] (/ (- v min-val) (- max-val min-val))) coll))))

(defn- apply-tensions
  [tension-vecs target-tensions values]
  {:pre (= (count tension-vecs) (count target-tensions))}
  (let [score (fn [target-tension tension-vec]
                (map (fwhen [t] (abs-diff target-tension t)) tension-vec))
        combine-tensions (fn [& args]
                            (reduce (fwhen [a b] (+ a b)) args))
        cost (fn [[_ score]]
               (if (nil? score) Integer/MAX_VALUE score))]
    (->> tension-vecs
         (map normalize)
         (map score target-tensions)
         (apply map combine-tensions)
         (map vector values)
         (min-by cost)
         first)))

(defn chord-set [tonic scale]
  (let [chord-shapes
        {6 [[0 2 4 7]]
         7 [[0 2 4 7] [0 2 4 6] [0 1 4 6] [0 3 4 6]]
         8 [[0 2 4 6] [0 2 4 7]]}]
    (for [shape (-> (scales scale) count chord-shapes)
          degree (range 1 8)]
      {:tonic tonic :scale scale
       :root ((pitch-scale tonic scale) (dec degree))
       :pitches (pitch-chord tonic scale shape degree)
       :notes (base-chord tonic scale shape degree)})))

(defn- find-chord-set
  [scales {:keys [tonic scale]} target-color]
  (let [scale-brightness (fn [tonic scale]
                           (->> (count (scherz.util/scales scale))
                                (/ (scale-brightness scale))
                                (+ (pitch-brightness tonic))))]
    (->> (interleave (fifths tonic :asc)
                     (fifths tonic :desc))
         (drop 1)
         (mapcat (fn [tonic] (map (partial vector tonic) scales)))
         (filter #(<= (- target-color 0.5)
                      (abs-diff (apply scale-brightness %)
                                (scale-brightness tonic scale))
                      (+ target-color 0.5)))
         first
         (apply chord-set))))

(scale-brightness :lydian)
(scale-brightness :diminished)

(defn generate-progression
  ([tensions scales] (generate-progression tensions scales :C))
  ([{:keys [color consonance gravity]} scales start-tonic]
   (reduce (fn [chord-progression position]
             (let [target-color (* 5 (color position))
                   prev-chord (peek chord-progression)
                   chords (find-chord-set scales prev-chord target-color)
                   color-t (map (fn [chord]
                                  (->> (:pitches chord)
                                       (chord-color (:pitches prev-chord))
                                       (abs-diff target-color)))
                                chords)
                   consonance-t (map #(chord-consonance (:notes %)) chords)
                   gravity-t (map #(chord-gravity (:notes prev-chord) (:notes %))
                                  chords)]
               (conj chord-progression
                     (apply-tensions [color-t consonance-t gravity-t]
                                     [0 (consonance position) (gravity position)]
                                     chords))))
           [(first (chord-set start-tonic (first scales)))]
           (range 0 (count color)))))

(defn voice-progression [progression]
  (let [constrain-voicing (fn [notes]
                            (cond (< (apply min notes) 60) (invert notes 1)
                                  (< 80 (apply max notes)) (invert notes -1)
                                  :else notes))
        voice-notes (fn [prev-chord chord]
                      (->> (:notes prev-chord)
                           (#(voice-lead % (:notes chord)))
                           constrain-voicing
                           (assoc chord :notes)))
        voice-pitches (fn [chord]
                        (->> (:notes chord)
                             (inversion (pitch->midi (:root chord)))
                             (rotate (:pitches chord))
                             (assoc chord :pitches)))
        initial-chord (->> (:notes (first progression))
                           (map (partial + 60))
                           constrain-voicing
                           (assoc (first progression) :notes)
                           voice-pitches)]
    (reductions (comp voice-pitches voice-notes)
                initial-chord
                (rest progression))))

(def main (comp voice-progression generate-progression))
