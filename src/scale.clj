(ns scherz.scale
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

(defn fifths
  ([root] (fifths root :asc))
  ([root direction]
   {:pre [(valid-direction? direction)]}
   (let [base-fifths (if (= direction :asc)
                       ["F" "C" "G" "D" "A" "E" "B"]
                       ["B" "E" "A" "D" "G" "C" "F"])
         shift-fn (if (= direction :asc) sharpen flatten)
         natural-root (subs (name root) 0 1)
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

(defn scale-brightness [mode]
  (let [cumulative-intervals (reductions + (SCALE mode))
        interval-brightness [0 -5 2 -3 4 -1 0 1 -4 3 -2 5]]
   (reduce (fn [acc interval]
             (+ acc (get interval-brightness interval 0)))
           0 cumulative-intervals)))

(defn circle-of-fifths
  ([root] (circle-of-fifths root :major))
  ([root mode]
   (let [bright? (pos? (scale-brightness mode))
         upper-arc (take (if bright? 6 5)
                         (drop 1 (fifths root)))
         lower-arc (take (if bright? 6 7)
                         (fifths root :desc))]
     (into upper-arc lower-arc))))

(circle-of-fifths :C :locrian)

(defn pitch-scale [root mode]
  (let [circle (vec (circle-of-fifths root mode))
        root-index (.indexOf circle root)]
    (->> (SCALE mode)
         (reductions +)
         (map #(-> % (* 7) (+ root-index) (mod 12)))
         (mapv circle)
         pop (into [root]))))

