(ns scherz.gravity-test
  (:require [clojure.test :refer [deftest is]]
            [scherz.gravity :refer [note-invert pitch-invert gravity]]))


(deftest inversions
  (is (= (note-invert [0 4 7 11] 2) [7 11 12 16]))
  (is (= (pitch-invert ["C" "E" "G" "C"] 1) ["E" "G" "C" "E"])))

(deftest chord-gravity
  (is (= (gravity '(0 4 7 11) '(0 4 7 11)) nil))
  (is (= (gravity '(0 3 7 11) '(0 2 7 10)) 1)))



