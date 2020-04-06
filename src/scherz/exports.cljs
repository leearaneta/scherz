(ns scherz.exports
  (:require
   [scherz.scale :refer [scales]]
   [scherz.chord :refer [possible-chord-types]]
   [scherz.brightness :refer [pitch->brightness brightness->pitch
                              fifths-above fifths-between]]
   [scherz.generate :refer [initial-chord generate-chords
                            generate-progression]]
   ))

(defn fnjs [f]
  (fn [& args]
    (clj->js (apply f (map #(js->clj % :keywordize-keys true) args)))))

(def util #js {:scales (clj->js scales)
               :possibleChordTypes (fnjs possible-chord-types)})
(def generate #js {:initialChord (fnjs initial-chord)
                   :generateChords (fnjs generate-chords)
                   :generateProgression (fnjs generate-progression)})

(def brightness #js {:pitchToBrightness (fnjs pitch->brightness)
                     :brightnessToPitch (fnjs brightness->pitch)
                     :fifthsAbove (fnjs fifths-above)
                     :fifthsBetween (fnjs fifths-between)})
