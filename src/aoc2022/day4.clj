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
  (bool2int (or (fully-contains datum)
                (fully-contains (reverse datum)))))

(defn total-score [data]
  (->> data (map score) (reduce +)))

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
  (total-score data) ;; => 532
  )

;; part2
;; In how many assignment pairs do the ranges overlap?

(defn sort-datum [[[f1 :as r1] [f2 :as r2]]]
  (if (<= f1 f2) [r1 r2] [r2 r1]))

(defn overlap? [datum]
  (let [[[_ t1] [f2 _]] (sort-datum datum)]
    (<= f2 t1)))

(defn total-score2 [data]
  (->> data (map overlap?) (map bool2int) (reduce +)))

(comment
  (map sort-datum (take-last 2 test-data))
  ;; => ([(4 6) (6 6)] [(2 6) (4 8)])

  (map overlap? test-data)
  ;; => (false false true true true true)

  (total-score2 test-data) ;; => 4
  (total-score2 data) ;; => 854
  )

