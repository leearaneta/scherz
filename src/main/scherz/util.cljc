(ns scherz.util)

(defmacro fwhen [args body]
  (let [a (if (some #(= '& %) args)
               `(concat ~(-> args vec pop pop) ~(last args))
               args)]
    `(fn ~args
       (if (some nil? ~a) nil ~body))))

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
      (do
        (when (< 1 (count elems)) (print (str (count elems) " possibilities\n")))
        elems)
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
