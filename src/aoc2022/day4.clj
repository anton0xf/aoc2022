(ns aoc2022.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]))
;; https://adventofcode.com/2022/day/4

;; part1
(defn parse-range [s]
  (->> (str/split s #"-")
       (map #(Integer/parseInt %))))

(defn parse-line [line]
  (->> (str/split line #",")
       (map parse-range)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)))

;; In how many assignment pairs does one range fully contain the other?

(defn fully-contains
  "return true if [f1 t1] fully contains [f2 t2]"
  [[[f1 t1] [f2 t2]]]
  (<= f1 f2 t2 t1))

(defn bool2int [b] (if b 1 0))

(defn score [datum]
  (+ (bool2int (fully-contains datum))
     (bool2int (fully-contains (reverse datum)))))

(defn total-score [data]
  (->> data (map eval-both-directions) (reduce +)))

(comment
  (def test-input
    "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")
  (def test-data (parse-input test-input))
  (map eval-both-directions test-data) ;; => (0 0 0 1 1 0)
  (total-score test-data) ;; => 2

  (def data (parse-input (slurp (io/resource "day4/input.txt"))))
  (total-score data) ;; => 567
  )
