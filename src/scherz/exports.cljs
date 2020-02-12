(ns scherz.exports
  (:require [scherz.scale :refer [scales]]
            [scherz.chord :refer [possible-chord-types]]
            [scherz.generate :refer [initial-chord generate-chords
                                     generate-progression]]))

(def util #js {:scales (clj->js scales)
               :possibleChordTypes (comp clj->js possible-chord-types)})
(def generate #js {:initialChord (comp clj->js initial-chord)
                   :generateChords (comp clj->js generate-chords)
                   :generateProgression (comp clj->js generate-progression)})
