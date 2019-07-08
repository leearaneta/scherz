(ns scherz.voicing
  (:use [overtone.live])
  (:require [scherz.brightness]))

(def degree->roman
  (zipmap (vals DEGREE) (keys DEGREE)))

(defn base-chord [tonic mode note-ct degree]
  (chord-degree (degree->roman degree)
                (-> tonic name (str -1) keyword)
                mode
                note-ct))

(defn compress [notes]
  (distinct (map (fn [x] (mod x 12)) notes)))

(def compressed-chord (comp compress base-chord))

(defn min-by [f coll]
  (:elem (reduce (fn [acc val]
                   (let [compare (f val)]
                     (if (< compare (:min acc))
                       {:elem val :min compare}
                       acc)))
                 {:elem (first coll) :min (f (first coll))}
                 (rest coll))))


(defn note-distance [current-note target-note]
  (let [diff (- target-note current-note)]
    (min-by #(Math/abs %)
            [diff (+ diff 12) (- diff 12)])))

(defn- vecs->map [vecs]
  (reduce (fn [acc [key val]]
            (let [acc-val (acc key)]
              (cond (nil? acc-val) (conj acc {key val})
                    (seq? acc-val) (conj acc {key (conj acc-val val)})
                    :else (conj acc {key [acc-val val]}))))
          {} vecs))

(defn- chord-transition [source-notes target-notes]
  (if (> (count target-notes) (count source-notes))
    (->> (chord-transition target-notes source-notes)
         (map (fn [[source distance]]
                [(mod (+ source distance) 12) (* -1 distance)])))
    (loop [transition {}
           sources-remaining source-notes
           targets-remaining target-notes]
      (cond (seq sources-remaining)
            (let [source (first sources-remaining)
                  cost (fn [[_ distance]] (Math/abs distance))
                  [target distance] (->> target-notes
                                         (map (partial note-distance source))
                                         (map vector target-notes)
                                         (min-by cost))]
              (recur (conj transition {source distance})
                     (next sources-remaining)
                     (remove (partial = target) targets-remaining)))
            (seq targets-remaining)
            (let [target (first targets-remaining)
                  cost (fn [[source distance]]
                         (-> (Math/abs distance) (* 2) (- (transition source))))
                  [source distance] (->> source-notes
                                         (map (partial note-distance target))
                                         (map vector source-notes)
                                         (min-by cost))]
              (recur (conj transition {source distance})
                     sources-remaining
                     (next targets-remaining)))
            :else (into [] transition)))))

(defn voice-chord [source-notes target-notes]
  (let [transition (vecs->map (chord-transition (compress source-notes)
                                                (compress target-notes)))
        voice-note (fn [note]
                    (let [compressed (mod note 12)
                          octaves (-> note (/ 12) (#(Math/floor %)) int)
                          distance (transition compressed)
                          apply-distance #(-> % (+ compressed) (+ (* octaves 12)))]
                      (if (vector? distance) ; if the note is going to multiple places
                        (map apply-distance distance)
                        (apply-distance distance))))]
    (flatten (map voice-note source-notes))))

(defn chord-distance [source-notes target-notes]
  (reduce (fn [total [_ distance]]
            (+ total (Math/abs distance)))
          0 (chord-transition source-notes target-notes)))

(defn pitch-chord [tonic mode note-ct degree]
  (->> (scherz.brightness/pitch-scale tonic mode)
       cycle
       (drop (dec degree))
       (take-nth 2)
       (take note-ct)))

(defn chord-gravity [source-notes target-notes]
  (let [transition (chord-transition (compress source-notes)
                                     (compress target-notes))
        filtered (filter (fn [[k v]] (not= v 0)) transition)
        square #(* % %)]
    (/ (->> (vals filtered) (map square) (reduce +)) ; square all values, add them up
       (count filtered))))

