(ns scherz.exports
  (:require [scherz.scales :refer [scales]]
            [scherz.brightness :refer [pitch-brightness scale-brightness
                                       circle-of-fifths
                                       fifths-between fifths-above]]
            [scherz.dissonance :refer [chord-dissonance scale-dissonance]]
            [scherz.chord :refer [possible-chord-types]]
            [scherz.generate :refer [initial-chord next-chord
                                     generate-progression]]))

(def scale #js {:scales (clj->js scales)})
(def brightness #js {:pitchBrightness pitch-brightness
                     :scaleBrightness scale-brightness
                     :circleOfFifths (comp clj->js circle-of-fifths)
                     :fifthsBetween (comp clj->js fifths-between)
                     :fifthsAbove fifths-above})
(def dissonance #js {:chordDissonance chord-dissonance
                     :scaleDissonance (clj->js scale-dissonance)})
(def chord #js {:possibleChordTypes (comp clj->js possible-chord-types)})
(def generate #js {:initialChord (comp clj->js initial-chord)
                   :nextChord (comp clj->js next-chord)
                   :generateProgression (comp clj->js generate-progression)})

