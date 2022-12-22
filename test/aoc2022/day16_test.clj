(ns aoc2022.day16-test
  (:require [aoc2022.day16 :refer :all]
            [clojure.test :refer :all]))

(deftest test-parse-line
  (is (= ["AA" 0 ["DD" "II" "BB"]]
         (parse-line "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB")))
  (is (= ["HH" 22 ["GG"]]
         (parse-line "Valve HH has flow rate=22; tunnel leads to valve GG"))))

