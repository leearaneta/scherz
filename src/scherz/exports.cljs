(ns scherz.exports
  (:require
   [scherz.scale :refer [scales]]
   [scherz.brightness :refer [pitch->brightness brightness->pitch
                              fifths-above fifths-between valid-pitch?]]
   [scherz.generate :refer [initial-chords generate-chords
                            generate-progression]]))

(defn fnjs [f]
  (fn [& args]
    (clj->js (apply f (map #(js->clj % :keywordize-keys true) args)))))

(def scherz #js {:initialChords (fnjs initial-chords)
                 :generateChords (fnjs generate-chords)
                 :generateProgression (fnjs generate-progression)
                 :scales (clj->js scales)})

(def util #js {:pitchToBrightness (fnjs pitch->brightness)
               :brightnessToPitch (fnjs brightness->pitch)
               :fifthsAbove (fnjs fifths-above)
               :fifthsBetween (fnjs fifths-between)
               :validatePitch (fnjs valid-pitch?)
               :scales (clj->js scales)})
