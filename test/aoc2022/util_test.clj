(ns aoc2022.util-test
  (:require  [aoc2022.util :refer :all]
             [clojure.test :refer :all]))

(deftest test-get-bounds
  (is (= [[-2 -2] [4 5]]
         (get-bounds 2 [[0 3] [2 0]]))))
