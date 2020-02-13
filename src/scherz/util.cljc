(ns scherz.util
  (:require #?(:clj [clojure.spec.alpha :as spec]
               :cljs [cljs.spec.alpha :as spec])))

(defn avg [coll]
  (if (empty? coll)
    nil
    (/ (reduce + coll)
       (count coll))))

(defn abs [v]
  (max v (- v)))

(def infinity
  #?(:clj Integer/MAX_VALUE
     :cljs js/Infinity))

(def abs-diff (comp abs -))

(defn floor [n]
  (Math/floor n))

(defn find-coll [f coll]
  (first (filter f coll)))

(defn min-by-coll [f coll]
  (loop [elems []
         min infinity
         coll coll]
    (if (empty? coll)
      elems
      (let [curr (first coll)
            compare (f curr)]
        (cond (< compare min)
              (recur [curr] compare (rest coll))
              (= compare min)
              (recur (conj elems curr) min (rest coll))
              :else
              (recur elems min (rest coll)))))))

(defn min-by [f coll]
  (get (min-by-coll f coll) 0 nil))

(defn max-by [f coll]
  (let [inverse (fn [v] (/ 1 v))]
    (min-by (comp inverse f) coll)))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f k v)])))

(defn rotate [coll]
  (cons (last coll) (drop-last coll)))

(defn insert [coll idx val]
  (let [[before after] (map vec (split-at idx coll))]
    (into (conj before val) after)))

(defn distinct-by
  ([f coll] (distinct-by f first coll))
  ([f g coll]
   (let [groups (group-by f coll)]
     (->> (distinct (map f coll))
          (map groups)
          (map g)))))

(defn extent [coll]
  [(apply min coll) (apply max coll)])

(spec/check-asserts true)
