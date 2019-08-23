(ns scherz.dissonance-test
  (:require [scherz.util])
  (:require [scherz.dissonance])
  (:use [clojure.test]))

(refer 'scherz.dissonance)

(deftest ratios
  (is (= (lcm-of-ratios [5/4 3/2]) 60)))
  (is (= (chord->ratios '(0 4 7)) [5/4 3/2]))

(deftest chord-dissonance
  (is (= (dissonance '(0 4 7)) 9))
  (is (= (dissonance '(0 4 7 11)) 10)))

(deftest avg-dissonance
  (is (< (scherz.util/avg (:lydian scale-dissonance))
         (scherz.util/avg (:diminished scale-dissonance)))))

