(ns scherz.brightness-test
  (:require [scherz.brightness]))

(use 'clojure.test)
(refer 'scherz.brightness)

(deftest shift
  (is (= (sharpen :Bb 3) :B##))
  (is (= (scherz.brightness/flatten :G# 2) :Gb)))

(deftest circle
  (is (= (fifths-above :F# 3) :D#))
  (is (= (fifths-above :Fb -3) :Abb)))

(deftest pitch
  (is (= (pitch-scale :D :lydian)
         '[:D :E :F# :G# :A :B :C#]))
  (is (= (pitch-chord :A :dorian '(0 2 4 6) 2)
         '(:B :D :F# :A))))

(deftest color
  (is (= (chord-color '(:C :E :G) '(:G :B :D)) 1))
  (is (= (chord-color '(:G :B :D :F) '(:C :E :G :C)) 0)))
