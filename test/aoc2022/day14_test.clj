(ns aoc2022.day14-test
  (:require [aoc2022.day14 :refer :all]
            [clojure.test :refer :all]))

(deftest test-parse-line
  (is (= '([498 4] [498 6] [496 6])
         (parse-line "498,4 -> 498,6 -> 496,6"))))


