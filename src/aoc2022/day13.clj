(ns aoc2022.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]))

(defn parse-groups [s]
  (->> (str/split s #"\n\n")
       (map (fn [gs] (->> (str/split-lines gs)
                          (map read-string))))))

(def test-input (-> (io/resource "day13/test.txt") slurp))
(def test-groups (parse-groups test-input))

(defn cmpl [l r]
  (match [l r]
         [[] []] 0
         [[] [_ & _]] -1
         [[_ & _] []] 1
         [[lh & lr] [rh & rr]] (let [cmph (cmpl lh rh)]
                                 (if (= 0 cmph) (recur lr rr) cmph))
         [(l :guard int?) (r :guard int?)] (compare l r)
         [(l :guard int?) [& r]] (recur [l] r)
         [[& l] (r :guard int?)] (recur l [r]))) 

(defn ordered? [l r] (> 0 (cmpl l r)))

(defn answer1 [pairs]
  (->> pairs
       (keep-indexed (fn [i v] (if (apply ordered? v) (inc i))))
       (reduce +)))

(defn parse-lists [s]
  (->> (str/split-lines s)
       (filter (comp not str/blank?))
       (map read-string)))

(def test-lists (parse-lists test-input))

(defn sort-lists [ls] (sort cmpl ls))

(defn first-index [x xs]
  (first (keep-indexed (fn [i v] (if (= x v) i)) xs)))

(defn answer2 [lists]
  (let [d1 [[2]], d2 [[6]]
        sorted (sort-lists (into lists [d1 d2]))
        n1 (inc (first-index d1 sorted))
        n2 (inc (first-index d2 sorted))]
    (* n1 n2)))

(comment
  test-data
  ;; => (([1 1 3 1 1], [1 1 5 1 1])
  ;;     ([[1] [2 3 4]], [[1] 4])
  ;;     ([9], [[8 7 6]])
  ;;     ([[4 4] 4 4], [[4 4] 4 4 4])
  ;;     ([7 7 7 7], [7 7 7])
  ;;     ([], [3])
  ;;     ([[[]]], [[]])
  ;;     ([1 [2 [3 [4 [5 6 7]]]] 8 9], [1 [2 [3 [4 [5 6 0]]]] 8 9]))

  (->> test-data
       (keep-indexed (fn [i v] (if (apply ordered? v) (inc i)))))
  ;; => (1 2 4 6)

  (answer1 test-data) ;; => 13

  (def input (slurp (io/resource "day13/input.txt")))
  (def groups (parse-groups input))
  (answer1 groups) ;; => 5393

  (answer2 test-lists) ;; => 140
  (def lists (parse-lists input))
  (answer2 lists) ;; => 26712
  )

