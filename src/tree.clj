(ns scherz.tree
  (:use [overtone.live])
  (:require [scherz.scale]))

(refer 'scherz.scale)
; let's make some chord progressions

(defn neo-riemann [[root shape]]
  (let [new-roots (->> (if (= shape :major) :asc :desc)
                       (fifths root)
                       (drop 3)
                       (take 2)
                       (cons root))
        new-shape (if (= shape :major) :minor :major)]
    (map vector new-roots (repeat 3 new-shape))))

(defn reptree [f val]
  {:value val
   :children (map (partial reptree f) (f val))})

(defn prune [n {:keys [value children]}]
  (if (= n 0) {:value value :children nil}
      {:value value
       :children (map (partial prune (dec n)) children)}))

(prune 3 (reptree neo-riemann [:A :major]))

(defn foldtree [f g a data]
  (cond (map? data)
        (let [{value :value children :children} data]
          (f value (foldtree f g a children)))
        (seq? data)
        (let [[child & rest] data]
          (g (foldtree f g a child) (foldtree f g a rest)))
        (nil? data) a))

(def depth (partial foldtree
                    (fn [_ x] (inc x))
                    (fn [x _] x)
                    0))

(defn maptree [f]
  (partial foldtree
           (fn [value children] {:value (f value) :children children})
           cons
           nil))

(depth (prune 5 (reptree neo-riemann [:A :major])))
