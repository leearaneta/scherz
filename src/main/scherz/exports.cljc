(ns scherz.exports
  (:require [scherz.scale :refer [scales]]
            [scherz.brightness :refer [pitch->brightness scale-brightness
                                       circle-of-fifths
                                       fifths-between fifths-above]]
            [scherz.dissonance :refer [chord-dissonance scale-dissonance]]
            [scherz.chord :refer [possible-chord-types]]
            [scherz.generate :refer [initial-chord generate-chords
                                     generate-progression]]))

(def util #js {:scales (clj->js scales)
               :possibleChordTypes (comp clj->js possible-chord-types)})
(def brightness #js {:pitchBrightness pitch->brightness
                     :scaleBrightness scale-brightness
                     :circleOfFifths (comp clj->js circle-of-fifths)
                     :fifthsBetween (comp clj->js fifths-between)
                     :fifthsAbove fifths-above})
(def dissonance #js {:chordDissonance chord-dissonance
                     :scaleDissonance (clj->js scale-dissonance)})
(def generate #js {:initialChord (comp clj->js initial-chord)
                   :generateChords (comp clj->js generate-chords)
                   :generateProgression (comp clj->js generate-progression)})
