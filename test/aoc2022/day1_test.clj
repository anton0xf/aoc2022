(ns aoc2022.day1-test
  (:require [aoc2022.day1 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(deftest test-max-calories
  (testing "Should work on example"
    (let [s (slurp (io/resource "day1/input.example.txt"))]
      (is (= 24000 (max-calories s))))))
