(ns aoc2022.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(defn div "divide string into two equal halfs" [s]
  (let [len (count s)
        h (quot len 2)]
    (assert (even? len) "s hould have even length")
    [(subs s 0 h) (subs s h)]))

(defn common [s]
  (->> (div s)
       (map set)
       (apply sets/intersection)))

(defn char-in [c from to]
  (<= (int from) (int c) (int to)))

(defn char-code [c min from]
  (+ min (- (int c) (int from))))

(defn priority [c]
  (cond
    (char-in c \a \z) (char-code c 1 \a)
    (char-in c \A \Z) (char-code c 27 \A)
    :else (assert false "Char should be english letter")))

(defn only [x]
  {:pre [(nil? (next x))]}
  (first x))

(defn total-priorities [lines]
  (->> (map common lines)
       (map only)
       (map priority)
       (reduce +)))

(comment
  (def test-input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")
  (def test-lines (str/split-lines test-input))
  (first test-lines)         ;; => "vJrwpWtwJgWrhcsFMMfFFhFp"
  (count (first test-lines)) ;; => 24
  (div "asdfgh")             ;; => ["asd" "fgh"]
  (div (first test-lines))   ;; => ["vJrwpWtwJgWr" "hcsFMMfFFhFp"]
  (->> (div (first test-lines)) (map set) (apply sets/intersection)) ;; => #{\p}
  (map common test-lines) ;; => (#{\p} #{\L} #{\P} #{\v} #{\t} #{\s})
  (->> (map common test-lines) (map first) (map priority)) ;; => (16 38 42 22 20 19)
  (->> (map common test-lines) (map first) (map priority) (reduce +)) ;; => 157
  (total-priorities (str/split-lines test-input)) ;; => 157

  (def data (str/split-lines (slurp (io/resource "day3/input.txt"))))
  (total-priorities data) ;; => 7766 
  )

;; part 2

(defn get-bage [lines]
  (->> lines (map set)
       (apply sets/intersection)
       only))

(defn total-priorities2 [lines]
  (->> lines (partition 3)
       (map get-bage)
       (map priority)
       (reduce +)))

(comment
  (->> test-lines (partition 3) first)
  ;; => ("vJrwpWtwJgWrhcsFMMfFFhFp"
  ;;     "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  ;;     "PmmdzqPrVvPwwTWBwg")

  (->> test-lines (partition 3) first
       (map set) (apply sets/intersection) only) ;; => \r
  (->> test-lines (partition 3)
       (map get-bage) (map priority)) ;; => (18 52)
  (total-priorities2 test-lines) ;; => 70

  (total-priorities2 data) ;; => 2415
  )
