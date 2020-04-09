(ns scherz.exports
  (:require
   [scherz.scale :refer [scales]]
   [scherz.brightness :refer [pitch->brightness brightness->pitch
                              fifths-above fifths-between]]
   [scherz.generate :refer [initial-chords generate-chords
                            generate-progression]]))

(defn fnjs [f]
  (fn [& args]
    (clj->js (apply f (map #(js->clj % :keywordize-keys true) args)))))

(def util #js {:scales (clj->js scales)})
(def generate #js {:initialChords (fnjs initial-chords)
                   :generateChords (fnjs generate-chords)
                   :generateProgression (fnjs generate-progression)})

(def brightness #js {:pitchToBrightness (fnjs pitch->brightness)
                     :brightnessToPitch (fnjs brightness->pitch)
                     :fifthsAbove (fnjs fifths-above)
                     :fifthsBetween (fnjs fifths-between)})
