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

(defn chord-set
  ([tonic mode] (chord-set tonic mode 4))
  ([tonic mode note-ct]
   (map (fn [degree]
          {:tonic tonic :mode mode
           :root ((pitch-scale tonic mode) (dec degree))
           :pitches (pitch-chord tonic mode note-ct degree)
           :notes (base-chord tonic mode note-ct degree)})
        (range 1 8))))

(defn- find-chord-set
  [modes {:keys [tonic pitches]} target-color]
  (let [scale-color (fn [chords]
                      (avg (map (partial chord-color pitches)
                                (map :pitches chords))))]
    (->> (interleave (fifths tonic :asc)
                     (fifths tonic :desc))
         (drop 1)
         (mapcat (fn [tonic] (map (partial chord-set tonic) modes)))
         (take (* 10 (count modes))) ; this is a lot
         (min-by (fn [chords]
                   (abs-diff target-color (scale-color chords)))))))

(defn generate-progression
  [{:keys [color consonance gravity]} modes start-tonic]
  (reduce (fn [chord-progression position]
            (let [target-color (* 5 (color position))
                  prev-chord (peek chord-progression)
                  chords (find-chord-set modes prev-chord target-color)
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
          [(first (chord-set start-tonic (first modes) 4))]
          (range 0 (count color))))

(defn voice-progression [progression]
  (let [adjust-voicing (fn [notes]
                         (cond (< (apply min notes) 60) (invert-voicing notes 1)
                               (< 78 (apply max notes)) (invert-voicing notes -1)
                               :else notes))
        initial-chord (:notes (first progression))
        initial-voicing (->> (second initial-chord) (+ 12)
                             (assoc (vec initial-chord) 1)
                             (map (partial + 60)) sort)]
    (reduce (fn [voiced-progression chord]
              (conj voiced-progression
                    (-> (peek voiced-progression)
                        (voice-lead (:notes chord))
                        adjust-voicing sort)))
            [initial-voicing]
            (rest progression))))

; TODO:
  ; redefine gravity to prioritize half step transitions
  ; extend voice leading algorithm to accomodate chords of different sizes
  ; programatically create tension curves
  ; given a scale and some midi notes, convert all notes to pitches
  ; port to cljs
