(ns aoc2022.day12-test
  (:require [aoc2022.day12 :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(deftest test-search-on-map
  (is (= [0 0] (->> test-input parse-input (search-on-map \S))))
  (is (= [2 5] (->> test-input parse-input (search-on-map \E)))))

(deftest test-map-size
  (is (= [5 8] (map-size (:map test-data)))))

(deftest test-neigbours
  (is (= [[0 1] [1 0]]
         (neighbours [0 0] [5 8])))
  (is (= [[0 2] [0 0] [1 1]]
         (neighbours [0 1] [5 8])))
  (is (= [[4 1] [3 0]]
         (neighbours [4 0] [5 8]))))

(deftest test-available-neigbours
  (is (= [[0 1] [1 0]]
         (available-neigbours [0 0] (:map test-data))))
  (is (= [[1 1] [0 0] [2 0]]
         (available-neigbours [1 0] (:map test-data))))
  (is (= [[1 0] [3 0]]
         (available-neigbours [2 0] (:map test-data))))
  (is (= [[4 1] [3 0]]
         (available-neigbours [4 0] (:map test-data)))))

(deftest test-bfs-on-map
  (is (= (str/trim "
v..v<<<<
>v.vv<<^
.v.v>E^^
.>v>>>^^
..>>>>>^")
         (->> (bfs-on-map test-data)
              (format-path (map-size (:map test-data)))))))
