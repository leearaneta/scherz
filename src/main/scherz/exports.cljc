(ns scherz.exports
  (:require [scherz.util :refer [scales]])
  (:require [scherz.brightness :refer [pitch-brightness scale-brightness
                                       circle-of-fifths fifths-between]])
  (:require [scherz.dissonance :refer [chord-dissonance scale-dissonance]])  
  (:require [scherz.generate :refer [main]]))

(def util #js {:scales (clj->js scales)})
(def brightness #js {:pitch-brightness pitch-brightness
                     :scale-brightness scale-brightness
                     :circle-of-fifths (comp clj->js circle-of-fifths)
                     :fifths-between (comp clj->js fifths-between)})
(def dissonance #js {:chord-dissonance chord-dissonance
                     :scale-dissonance (clj->js scale-dissonance)})
(def generate #js {:generate (comp clj->js main)})
