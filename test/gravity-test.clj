(ns scherz.gravity-test
  (:use [clojure.test])
  (:require [scherz.gravity]))

(refer 'scherz.gravity)

(deftest inversions
  (is (= (invert '(12 16 19 23) -2) '(7 11 12 16)))
  (is (= (inversion 0 '(19 23 24 28)) 2)))

(deftest gravity
  (is (= (chord-gravity '(0 4 7 11) '(0 4 7 11)) nil))
  (is (= (chord-gravity '(0 4 7 11) '(0 4 7 12)) 1)))



