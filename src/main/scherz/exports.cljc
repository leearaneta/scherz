(ns scherz.exports
  (:require [scherz.util :refer [scales]])
  (:require [scherz.brightness :refer [pitch-brightness scale-brightness
                                       circle-of-fifths fifths-between]])
  (:require [scherz.dissonance :refer [chord-dissonance scale-dissonance]])  
  (:require [scherz.generate :refer [initial-chord next-chord
                                     generate-progression possible-types]]))

(def util #js {:scales (clj->js scales)})
(def brightness #js {:pitchBrightness pitch-brightness
                     :scaleBrightness scale-brightness
                     :circleOfFifths (comp clj->js circle-of-fifths)
                     :fifthsBetween (comp clj->js fifths-between)})
(def dissonance #js {:chordDissonance chord-dissonance
                     :scaleDissonance (clj->js scale-dissonance)})
(def generate #js {:possibleTypes (comp clj->js possible-types)
                   :initialChord (comp clj->js initial-chord)
                   :nextChord (comp clj->js next-chord)
                   :generateProgression (comp clj->js generate-progression)})
