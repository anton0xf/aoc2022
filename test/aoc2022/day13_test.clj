(ns aoc2022.day13-test
  (:require [aoc2022.day13 :refer :all]
            [clojure.test :refer :all]))

(deftest test-cmpl
  (is (= -1 (cmpl 1 2)))
  (is (=  0 (cmpl 1 1)))
  (is (=  1 (cmpl 2 1)))
  (is (=  0 (cmpl [] [])))
  (is (= -1 (cmpl [] [1])))
  (is (= -1 (cmpl [] [1])))
  (is (=  0 (cmpl [1] [1])))
  (is (= -1 (cmpl [1] [1 2])))
  (is (=  1 (cmpl [2] [1 3])))
  (is (=  1 (cmpl [1 2] [1])))
  (is (=  0 (cmpl 1 [1])))
  (is (=  0 (cmpl [1] 1)))
  (is (= -1 (cmpl [1 2] 3)))
  (is (= -1 (cmpl 1 [2])))
  (is (= -1 (cmpl 1 [1 2])))
  (is (=  1 (cmpl 3 [1 2])))
  (is (= -1 (cmpl [1 1 3 1 1] [1 1 5 1 1])))
  (is (= -1 (cmpl [[1] [2 3 4]] [[1] 4])))
  (is (=  0 (cmpl [[1] []] [[1] []])))
  (is (= -1 (cmpl [[1] [2 3 4]] [[1] 4])))
  (is (= -1 (cmpl [[4 4] 4 4] [[4 4] 4 4 4])))
  (is (=  1 (cmpl [7 7 7 7] [7 7 7])))
  (is (=  1 (cmpl [[[]]] [[]])))
  (is (=  1 (cmpl [1 [2 [3 [4 [5 6 7]]]] 8 9] [1 [2 [3 [4 [5 6 0]]]] 8 9])))
  (is (=  0 (cmpl [1 [2 [3 [4 [5 6 0]]]] 8 9] [1 [2 [3 [4 [5 6 0]]]] 8 9]))))

(deftest test-ordered
  (is (ordered? 1 2))
  (is (not (ordered? 1 1)))
  (is (not (ordered? 2 1)))
  (is (not (ordered? [] [])))
  (is (ordered? [] [1]))
  (is (ordered? [] [1]))
  (is (ordered? [1] [1 2]))
  (is (not (ordered? [2] [1 3])))
  (is (not (ordered? [1 2] [1]))))

(deftest test-answer1
  (is (= 13 (answer1 test-groups))))

(deftest test-sort-input
  (is (= [[]
          [[]]
          [[[]]]
          [1 1 3 1 1]
          [1 1 5 1 1]
          [[1] [2 3 4]]
          [1 [2 [3 [4 [5 6 0]]]] 8 9]
          [1 [2 [3 [4 [5 6 7]]]] 8 9]
          [[1] 4]
          [[2]]
          [3]
          [[4 4] 4 4]
          [[4 4] 4 4 4]
          [[6]]
          [7 7 7]
          [7 7 7 7]
          [[8 7 6]]
          [9]]
         (sort-lists (into test-lists [[[2]] [[6]]])))))

(deftest test-first-index
  (is (= nil (first-index 1 [2 3])))
  (is (= 0 (first-index 1 [1 2 3])))
  (is (= 1 (first-index 1 [2 1 3])))
  (is (= 2 (first-index 1 [2 3 1]))))
