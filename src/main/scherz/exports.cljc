(ns scherz.exports
  (:require [scherz.brightness :refer [pitch-brightness circle-of-fifths]])
  (:require [scherz.generate :refer [main]])
  (:require [scherz.util :refer [scale-intervals]]))

(defn add-spiral [progression]
  (let [add-circle (fn [{:keys [tonic scale] :as chord}]
                     (into chord
                           {:circle (circle-of-fifths tonic scale)}))
        add-sorted (fn [{:keys [pitches] :as chord}]
                     (into chord
                           {:sorted (dedupe (sort-by pitch-brightness pitches))}))
        progression (->> progression (map add-circle) (map add-sorted))
        spiral (->> progression
                    (mapcat :circle)
                    (sort-by pitch-brightness)
                    distinct)]
    #?(:clj {:spiral spiral :progression progression}
       :cljs (clj->js {:spiral spiral :progression progression}))))

(def generate (comp add-spiral main))
(def scales #?(:clj (keys scale-intervals)
               :cljs (clj->js (keys scale-intervals))))


