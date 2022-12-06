(ns aoc2022.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(def marker-size 4)

(defn search-marker [s]
  (->> s
       (partition marker-size 1)
       (keep-indexed
        (fn [idx part]
          (if (= marker-size (count (set part)))
            [(+ marker-size idx) (str/join part)])))
       first))

(comment
  (def test-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
  (search-marker test-input) ;; => [7 "jpqm"]
  
  (search-marker "bvwbjplbgvbhsrlpgdmjqwftvncz") ;; => [5 "vwbj"]
  (search-marker "nppdvjthqldpwncqszvftbrmjlhg") ;; => [6 "pdvj"]
  (search-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;; => [10 "rfnt"]
  (search-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ;; => [11 "zqfr"]

  (def input (slurp (io/resource "day6/input.txt")))
  (search-marker input) ;; => [1647 "dsjt"]
  )

