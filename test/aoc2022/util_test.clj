(ns aoc2022.util-test
  (:require [aoc2022.util :refer :all]
            [clojure.test :refer :all]))

(deftest test-bounds
  (is (= [[-1 -1] [3 5]]
         (bounds 1 [[0 4] [2 0]])))
  (is (= [[-2 -2] [2 2]]
         (bounds 2 [[0 0]])))
  (is (= [[-2 -2] [4 5]]
         (bounds 2 [[0 3] [2 0]]))))

(deftest test-direction-to
  (is (= [0 0] (direction-to [0 0] [0 0])))

  (is (= [0 1] (direction-to [0 0] [0 1])))
  (is (= [0 1] (direction-to [0 0] [0 3])))
  (is (= [0 -1] (direction-to [0 0] [0 -1])))
  (is (= [0 -1] (direction-to [0 1] [0 -5])))

  (is (= [1 0] (direction-to [0 0] [1 0])))
  (is (= [1 0] (direction-to [0 0] [3 0])))
  (is (= [-1 0] (direction-to [0 0] [-1 0])))
  (is (= [-1 0] (direction-to [0 0] [-5 0])))

  (is (= [1 1] (direction-to [0 0] [1 1])))
  (is (= [1 1] (direction-to [0 0] [1 3])))
  (is (= [1 1] (direction-to [0 0] [3 1])))
  
  (is (= [1 -1] (direction-to [0 0] [1 -1])))
  (is (= [1 -1] (direction-to [0 0] [1 -5])))
  (is (= [1 -1] (direction-to [0 0] [5 -1])))

  (is (= [-1 1] (direction-to [0 0] [-1 1])))
  (is (= [-1 1] (direction-to [0 0] [-5 1])))
  (is (= [-1 1] (direction-to [0 0] [-1 5])))

  (is (= [-1 -1] (direction-to [0 0] [-1 -1])))
  (is (= [-1 -1] (direction-to [0 0] [-5 -1])))
  (is (= [-1 -1] (direction-to [0 0] [-1 -5]))))
