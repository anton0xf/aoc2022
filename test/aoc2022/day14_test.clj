(ns aoc2022.day14-test
  (:require [aoc2022.day14 :refer :all]
            [clojure.test :refer :all]))

(deftest test-trace-segment
  (is (= [[1 4] [1 5] [1 6]] (trace-segment [1 4] [1 6]))))

(deftest test-trace-path
  (is (= '([1 4] [1 5] [1 6] [2 6] [3 6])
         (trace-path '([1 4] [1 6] [3 6]))))
  (is (= '([498 4] [498 5] [498 6] [497 6] [496 6])
         (trace-path '([498 4] [498 6] [496 6])))))

(def simple-map
  (paths-to-map [1 -3] [[[0 0] [2 0]]]))

(deftest test-paths-to-map
  (is (= {:bounds [[-1 -1] [3 3]]
          :source [1 2]
          :sand 0
          :ps {[1 2] \+, [0 0] \#, [1 0] \#, [2 0] \#}}
         (paths-to-map [1 2] [[[0 0] [2 0]]])))
  (is (= "sand: 0
.....
..+..
.....
.....
.###.
....."
         (->> simple-map map-to-str))))

(deftest test-sand-fall
  (is (= "sand: 1
.....
..+..
.....
..o..
.###.
....."
         (->> simple-map sand-fall map-to-str)))
  (is (= "sand: 1
.....
..+..
..~..
.~o..
~###.
~...."
         (->> simple-map sand-fall sand-fall map-to-str))))
