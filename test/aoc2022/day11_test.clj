(ns aoc2022.day11-test
  (:require [aoc2022.day11 :refer :all]
            [clojure.test :refer :all]))

(deftest test-parse-monkey
  (is (= {:num 7,
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
