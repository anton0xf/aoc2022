(ns aoc2022.day11-test
  (:require [aoc2022.day11 :refer :all]
            [clojure.test :refer :all]))

(deftest test-parse-monkey
  (is (= {:id 7,
          :items [79 98],
          :operation ["*" 19],
          :test [:divisible-by 23],
          [:if true] 2,
          [:if false] 3}
         (parse-monkey ["Monkey 7:"
                        "  Starting items: 79, 98"
                        "  Operation: new = old * 19"
                        "  Test: divisible by 23"
                        "    If true: throw to monkey 2"
                        "    If false: throw to monkey 3"]))))

(deftest test-inspect-item
  (is (= 5 (inspect-item 2 {:operation ["+" 3]})))
  (is (= 6 (inspect-item 2 {:operation ["*" 3]})))
  (is (= 4 (inspect-item 2 {:operation ["*" :old]}))))

(deftest test-test-intem
  (is (test-item 2080 {:test [:divisible-by 13]})))

(deftest test-count-inspected
  (is (= [[0 101] [1 95] [2 7] [3 105]]
         (count-inspected (first test-data) (second test-data) 20))))

(deftest test-answer1
  (is (= 10605 (answer1 test-data))))
