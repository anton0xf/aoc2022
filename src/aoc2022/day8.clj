(ns aoc2022.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(defn char-to-int [c]
  (- (int c) (int \0)))

(defn parse-input [s]
  (->> (str/split-lines s)
       (mapv vec)
       (mapv #(mapv char-to-int %))))

(defn get-dimensions [m]
  [(count m) (count (first m))])

(defn get-row [i [dim-i dim-j]]
  (->> (range dim-j)
       (mapv (fn [j] [i j]))))

(defn get-rows [[dim-i dim-j :as dim] row-f]
  (->> (range dim-i)
       (map #(row-f (get-row % dim)))))

(defn get-col [j [dim-i dim-j]]
  (->> (range dim-i)
       (mapv (fn [i] [i j]))))

(defn get-cols [[dim-i dim-j :as dim] col-f]
  (->> (range dim-j)
       (map #(col-f (get-col % dim)))))

(defn filter-invisible [m ps]
  (loop [res '(), h -1, ps ps]
    (if (seq ps)
      (let [[p & rps] ps, val (get-in m p)]
        (recur (if (<= val h) (conj res p) res)
               (if (> val h) val h)
               rps))
      (set res))))

(defn get-invisible-by [m pss-gen-f]
  (let [dim (get-dimensions m)
        pss (pss-gen-f dim)]
    (->> (map (fn [ps] (filter-invisible m ps)) pss)
         (reduce into #{}))))

(defn get-invisible [m]
  (sets/intersection
   (get-invisible-by m #(get-rows % identity))
   (get-invisible-by m #(get-rows % reverse))
   (get-invisible-by m #(get-cols % identity))
   (get-invisible-by m #(get-cols % reverse))))

(defn answer1 [m]
  (let [[dim-i dim-j] (get-dimensions m)
        total (* dim-i dim-j)
        invisible (count (get-invisible m))]
    (- total invisible)))

(comment
  (def test-input
    "30373
25512
65332
33549
35390")
  
  (def test-data (parse-input test-input))
  test-data
  ;; => [[3 0 3 7 3] [2 5 5 1 2] [6 5 3 3 2] [3 3 5 4 9] [3 5 3 9 0]]
  (get-dimensions test-data) ;; => [5 5]
  (get-row 2 [5 6])        ;; => [[2 0] [2 1] [2 2] [2 3] [2 4] [2 5]]
  (get-col 3 [5 6])        ;; => [[0 3] [1 3] [2 3] [3 3] [4 3]]
  (get-in test-data [1 3]) ;; => 1
  (get-in test-data [4 4]) ;; => 0
  
  (let [m test-data
        dim (get-dimensions m)
        ps (get-row 1 dim)]
    (filter-invisible m ps))
  ;; => #{[1 4] [1 3] [1 2]}

  (get-invisible-by test-data #(get-rows % reverse))
  ;; => #{[2 2] [0 0] [1 0] [3 3] [1 1] [4 2] [3 0] [4 1] [1 3] [0 2] [3 1] [3 2] [0 1] [4 0]}

  (get-invisible-by test-data #(get-cols % identity))
  ;; => #{[2 2] [1 0] [2 3] [3 3] [4 2] [3 0] [4 1] [1 4] [1 3] [2 4] [3 1] [2 1] [4 4] [3 2] [4 0]}

  (get-invisible test-data) ;; => #{[2 2] [3 3] [1 3] [3 1]}

  (answer1 test-data) ;; => 21

  (def data (parse-input (slurp (io/resource "day8/input.txt"))))
  (answer1 data) ;; => 1812
  )



