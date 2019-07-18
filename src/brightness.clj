(ns scherz.brightness
  (:require [clojure.string])
  (:require [scherz.util]))

(defn valid-direction? [direction]
  (or (= direction :asc) (= direction :desc)))

(defn shift-pitch
  ([direction pitch] (shift-pitch direction pitch 1))
  ([direction pitch amt]
   {:pre [(valid-direction? direction)]}
   (if (== amt 0) (keyword pitch)
       (let [pitch (name pitch)
             pop-string (fn [s] (subs s 0 (- (count s) 1)))
             to-remove (if (= direction :asc) "b" "#")
             to-add (if (= direction :asc) "#" "b")]
         (if (clojure.string/ends-with? pitch to-remove)
           (recur direction (pop-string pitch) (- amt 1))
           (recur direction (str pitch to-add) (- amt 1)))))))

(def sharpen (partial shift-pitch :asc))
(def flatten (partial shift-pitch :desc))

(def base-circle [\F \C \G \D \A \E \B])

(defn fifths
  ([root] (fifths root :asc))
  ([root direction]
   {:pre [(valid-direction? direction)]}
   (let [base-fifths (if (= direction :asc)
                       base-circle
                       (reverse base-circle))
         shift-fn (if (= direction :asc) sharpen flatten)
         natural-root (first (name root))
         initial-accidental (subs (name root) 1)
         shift (fn [index value]
                 (->> (/ index 7)
                      (#(Math/floor %))
                      (shift-fn value)))]
     (lazy-seq (->> base-fifths
                    (map #(keyword (str % initial-accidental)))
                    cycle
                    (map-indexed shift)
                    (drop (.indexOf base-fifths natural-root)))))))

(def base-interval-brightness [0 -5 2 -3 4 -1 0 1 -4 3 -2 5 0])

(defn- scale-brightness [scale]
  (let [cumulative-intervals (reductions + (scherz.util/scales scale))
        base-brightness (reduce (fn [acc interval]
                                  (+ acc (base-interval-brightness interval)))
                                0 cumulative-intervals)]
    (if (some #(= 6 %) cumulative-intervals)
      (if (pos? base-brightness)
        (+ base-brightness 6)
        (- base-brightness 6))
      base-brightness)))

(defn circle-of-fifths
  ([root] (circle-of-fifths root :major))
  ([root scale]
   (let [bright? (pos? (scale-brightness scale))
         upper-arc (take (if bright? 6 5)
                         (drop 1 (fifths root)))
         lower-arc (take (if bright? 6 7)
                         (fifths root :desc))]
     (into upper-arc lower-arc))))

(defn pitch-scale [tonic scale]
  (let [circle (vec (circle-of-fifths tonic scale))
        root-index (.indexOf circle tonic)]
    (->> (scherz.util/scales scale)
         (reductions +)
         (map #(-> % (* 7) (+ root-index) (mod 12)))
         (mapv circle)
         pop
         (into [tonic]))))

(defn pitch-chord [tonic scale note-ct degree]
  (->> (pitch-scale tonic scale)
       cycle
       (drop (dec degree))
       (take-nth 2)
       (take note-ct)))

(defn pitch-brightness [pitch]
  (let [p (name pitch)
        counts (frequencies p)]
    (+ (.indexOf base-circle (first p))
       (* 7 (get counts \# 0))
       (* -7 (get counts \b 0)))))

(defn chord-color [source-pitches target-pitches]
  (let [chord-brightness (fn [pitches]
                           (map pitch-brightness pitches))
        brightest-note (fn [pitches]
                         (apply max (chord-brightness pitches)))
        darkest-note (fn [pitches]
                       (apply min (chord-brightness pitches)))
        brightness-difference (- (brightest-note target-pitches)
                                 (brightest-note source-pitches))
        darkness-difference (- (darkest-note source-pitches)
                               (darkest-note target-pitches))]
    (+ (max brightness-difference 0)
       (max darkness-difference 0))))
