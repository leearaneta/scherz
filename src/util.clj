(ns scherz.util
  (:require [overtone.live]))

(refer 'overtone.live)

(defmacro fwhen [args body]
  (let [new-body `(if (some nil? ~args) nil ~body)]
    `(fn ~args ~new-body)))

(defn avg [coll]
  (/ (reduce + coll)
     (count coll)))

(defn absv [v]
  (max v (- v)))

(def abs-diff (comp absv -))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))
 
(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v] (reduce lcm v))

(def degree->roman
  (zipmap (vals DEGREE)
          (keys DEGREE)))

(defn adjust-pitch [pitch]
  (if (< 1 (count (name pitch)))
    (let [pitch (name pitch)
          multiplier (if (= \# (last pitch)) 1 -1)]
      (-> pitch count dec
          (* multiplier)
          (+ (-> pitch first str keyword NOTES))
          (mod 12)
          REVERSE-NOTES))
    pitch))

(defn base-chord [tonic mode note-ct degree]
  (chord-degree (degree->roman degree)
                (-> tonic adjust-pitch name (str -1) keyword)
                mode note-ct))

(defn compress [notes]
  (sort (map (fn [x] (mod x 12)) notes)))

(defn min-by [f coll]
  (:elem (reduce (fn [acc val]
                   (let [compare (f val)]
                     (if (< compare (:min acc))
                       {:elem val :min compare}
                       acc)))
                 {:elem (first coll) :min (f (first coll))}
                 (rest coll))))
