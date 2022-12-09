(ns aoc2022.day8-test
  (:require [aoc2022.day8 :refer :all]
            [clojure.test :refer :all]))

(def m (parse-input "30373
25512
65332
33549
35390"))

(deftest test-distance-by-direction
  (is (= 1 (distance-by-direction m [1 2] '([1 1] [1 0]))))
  (is (= 2 (distance-by-direction m [1 2] '([1 3] [1 4]))))
  (is (= 0 (distance-by-direction m [1 0] '()))))

