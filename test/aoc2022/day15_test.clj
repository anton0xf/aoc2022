(ns aoc2022.day15-test
  (:require [aoc2022.day15 :refer :all]
            [clojure.test :refer :all]))

(deftest test-parse-line
  (is (= [[2 18] [-2 15]]
         (parse-line "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"))))

(deftest test-md
  (is (= 1 (md [0 0] [0 1])))
  (is (= 1 (md [0 0] [0 -1])))
  (is (= 1 (md [0 0] [1 0])))
  (is (= 1 (md [0 0] [-1 0])))
  (is (= 1 (md [0 1] [0 0])))
  (is (= 1 (md [0 -1] [0 0])))
  (is (= 1 (md [1 0] [0 0])))
  (is (= 1 (md [-1 0] [0 0])))
  (is (= 5 (md [1 1] [3 4]))))

(deftest test-interval-at-y
  (is (nil? (interval-at-y 10 [2 18] [-2 15])))
  (is (= [-2 2] (interval-at-y 10 [0 11] [2 10])))
  (is (= [-4 4] (interval-at-y 3 [0 0] [2 5])))
  (is (= [-4 4] (interval-at-y -3 [0 0] [2 5])))
  (is (= [-4 4] (interval-at-y 3 [0 0] [-2 5])))
  (is (= [-4 4] (interval-at-y 3 [0 0] [2 -5])))
  (is (= [-3 5] (interval-at-y 3 [1 0] [3 5])))
  (is (= [-4 4] (interval-at-y 3 [0 0] [5 2])))
  (is (= [-3 5] (interval-at-y 3 [1 0] [2 6]))))

(deftest test-merge-intervals-count0
  (is (= 8 (merge-intervals-count0 '([0 0] [2 5] [2 6] [8 9]))))
  (is (= 4 (merge-intervals-count0 '([2 5] [2 5]))))
  (is (= 4 (merge-intervals-count0 '([2 5] [2 3]))))
  (is (= 4 (merge-intervals-count0 '([2 5] [3 5]))))
  (is (= 4 (merge-intervals-count0 '([2 5] [3 4]))))
  (is (= 4 (merge-intervals-count0 '([2 5] [2 5] [3 4] [3 5] [2 4]))))
  (is (= 7 (merge-intervals-count0 '([0 4] [2 6] [3 5]))))
  (is (every? #(= 7 %)
              (for [l (range 0 7), r (range l 7)]
                (merge-intervals-count0 (list [0 4] [2 6] [l r])))))
  (is (= 27 (->> test-data (intervals-at-y 10) merge-intervals-count0))))

(deftest test-merge-intervals
  (is (= '([0 1]) (merge-intervals '([0 1]))))
  (is (= '([0 0]) (merge-intervals '([0 0]))))
  (is (= '([0 2]) (merge-intervals '([0 1] [1 2]))))
  (is (= '([0 2]) (merge-intervals '([0 2] [1 1]))))

  (is (= '([0 0] [2 6] [8 9]) (merge-intervals '([0 0] [2 5] [2 6] [8 9]))))
  (is (= '([2 5]) (merge-intervals '([2 5] [2 5]))))
  (is (= '([2 5]) (merge-intervals '([2 5] [2 3]))))
  (is (= '([2 5]) (merge-intervals '([2 5] [3 5]))))
  (is (= '([2 5]) (merge-intervals '([2 5] [3 4]))))
  (is (= '([2 5]) (merge-intervals '([2 5] [2 5] [3 4] [3 5] [2 4]))))
  (is (= '([0 6]) (merge-intervals '([0 4] [2 6] [3 5]))))
  (is (every? #(= '([0 6]) %)
              (for [l (range 0 7), r (range l 7)]
                (merge-intervals (list [0 4] [2 6] [l r])))))
  (is (= '([-2 24]) (->> test-data (intervals-at-y 10) merge-intervals))))

(deftest test-merge-intervals-count
  (is (= 8 (merge-intervals-count '([0 0] [2 5] [2 6] [8 9]))))
  (is (= 4 (merge-intervals-count '([2 5] [2 5]))))
  (is (= 4 (merge-intervals-count '([2 5] [2 3]))))
  (is (= 4 (merge-intervals-count '([2 5] [3 5]))))
  (is (= 4 (merge-intervals-count '([2 5] [3 4]))))
  (is (= 4 (merge-intervals-count '([2 5] [2 5] [3 4] [3 5] [2 4]))))
  (is (= 7 (merge-intervals-count '([0 4] [2 6] [3 5]))))
  (is (every? #(= 7 %)
              (for [l (range 0 7), r (range l 7)]
                (merge-intervals-count (list [0 4] [2 6] [l r])))))
  (is (= 27 (->> test-data (intervals-at-y 10) merge-intervals-count))))

(deftest test-answer1
  (is (= 26 (answer1 10 test-data))))
