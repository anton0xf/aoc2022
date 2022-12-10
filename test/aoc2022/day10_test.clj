(ns aoc2022.day10-test
  (:require [aoc2022.day10 :refer :all]
            [clojure.test :refer :all]))

(deftest test-parse-input
  (is (= [[0 ["noop"]] [1 ["addx" 3]] [2 ["addx" -5]]]
         (parse-input test-input))))

(deftest test-apply-cmds
  (is (= {:xs [[1 1] [2 1] [3 1] [4 4] [5 4]], :next-x -1}
         (apply-cmds 1 (parse-input test-input))))
  (is (= [[20 21] [60 19] [100 18] [140 21] [180 16] [220 18]]
         (->> test-data2 (apply-cmds 1)
              :xs get-key-points))))

(deftest test-answer1
  (is (= 13140 (answer1 test-data2))))

(deftest test-answer2
  (is (= "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."
         (answer2 test-data2))))
