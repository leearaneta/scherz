(ns scherz.dissonance-test
  (:require [clojure.test :refer [deftest is]] 
            [scherz.dissonance :refer [notes->ratios ratios->terms dissonance]]))

(deftest ratios
  (is (= (notes->ratios '(0 4 7))
         [{:numerator 1 :denominator 1}
          {:numerator 5 :denominator 4}
          {:numerator 3 :denominator 2}])))
  (is (= (ratios->terms [{:numerator 1 :denominator 1}
                         {:numerator 5 :denominator 4}
                         {:numerator 3 :denominator 2}])
         '(4 5 6)))

(deftest chord-dissonance
  (is (= (dissonance '(0 4 7)) 8))
  (is (= (dissonance '(0 4 7 11)) 9))
  (is (= (dissonance '(0 3 7 11)) 14)))
