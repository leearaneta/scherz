(ns scherz.brightness-test
  (:require [clojure.test :refer [deftest is]]
            [scherz.brightness :refer [pitch->brightness brightness->pitch
                                       circle-of-fifths pitch-scale
                                       fifths-above fifths-between]]))

(deftest conversion
  (is (= (pitch->brightness "Db") -5))
  (is (= (brightness->pitch 6) "F#")))

(deftest circle
  (is (= (circle-of-fifths "Ab")
         '("Bbb" "Fb" "Cb" "Gb" "Db" "Ab" "Eb" "Bb" "F" "C" "G" "D")))
  (is (= (pitch-scale "B" :minor)
         '("B" "C#" "D" "E" "F#" "G" "A"))))

(deftest distance
  (is (= (fifths-above 3 "F") "D"))
  (is (= (fifths-between "G" "E") '("G" "D" "A" "E"))))
