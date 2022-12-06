(ns aoc2022.day6-test
  (:require [aoc2022.day6 :refer :all]
            [clojure.test :refer :all]))

(deftest test-search-start-of-packet-marker
  (is (= [7  "jpqm"] (search-start-of-packet-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
  (is (= [5  "vwbj"] (search-start-of-packet-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")))
  (is (= [6  "pdvj"] (search-start-of-packet-marker "nppdvjthqldpwncqszvftbrmjlhg")))
  (is (= [10 "rfnt"] (search-start-of-packet-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
  (is (= [11 "zqfr"] (search-start-of-packet-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))))

(deftest test-search-start-of-message-marker
  (is (= 19 (first (search-start-of-message-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))))
  (is (= 23 (first (search-start-of-message-marker "bvwbjplbgvbhsrlpgdmjqwftvncz"))))
  (is (= 23 (first (search-start-of-message-marker "nppdvjthqldpwncqszvftbrmjlhg"))))
  (is (= 29 (first (search-start-of-message-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))))
  (is (= 26 (first (search-start-of-message-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))))
