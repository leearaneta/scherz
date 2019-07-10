(ns scherz.brightness
  (:use [overtone.live])
  (:require [clojure.string]))

(defn pop-string [s]
  (subs s 0 (- (count s) 1)))

(defn valid-direction? [direction]
  (or (= direction :asc) (= direction :desc)))

(defn shift-pitch
  ([direction pitch] (shift-pitch direction pitch 1))
  ([direction pitch amt]
   {:pre [(valid-direction? direction)]}
   (if (== amt 0) (keyword pitch)
       (let [pitch (name pitch)
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

(defn scale-brightness [mode]
  (let [cumulative-intervals (reductions + (SCALE mode))
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
  ([root mode]
   (let [bright? (pos? (scale-brightness mode))
         upper-arc (take (if bright? 6 5)
                         (drop 1 (fifths root)))
         lower-arc (take (if bright? 6 7)
                         (fifths root :desc))]
     (into upper-arc lower-arc))))

(defn pitch-scale [tonic mode]
  (let [circle (vec (circle-of-fifths tonic mode))
        root-index (.indexOf circle tonic)]
    (->> (SCALE mode)
         (reductions +)
         (map #(-> % (* 7) (+ root-index) (mod 12)))
         (mapv circle)
         pop (into [tonic]))))

(defn pitch-brightness [pitch]
  (let [p (name pitch)
        counts (frequencies p)]
    (+ (.indexOf base-circle (first p))
       (* 7 (get counts \# 0))
       (* -7 (get counts \b 0)))))

(defn chord-color [source-pitches target-pitches]
  (let [chord-brightness (fn [pitches]
                           (map pitch-brightness pitches))
        brightest-note (fn [pitches] (max (chord-brightness pitches)))
        darkest-note (fn [pitches] (min (chord-brightness pitches)))
        brightness-difference (- (brightest-note target-pitches)
                                 (brightest-note source-pitches))
        darkness-difference (- (darkest-note source-pitches)
                               (darkest-note target-pitches))]
    (+ (apply max brightness-difference 0)
       (apply max darkness-difference 0))))
