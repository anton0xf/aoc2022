(ns aoc2022.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-calories [s]
  (->> (str/split-lines s)
       (partition-by #(= % ""))
       (filter #(not= % '("")))
       (map (fn [xs] (reduce + (map #(Integer/parseInt %) xs))))))

(defn max-calories [xs]
  (reduce max xs))

(defn top3 [xs]
  (->> xs (sort >) (take 3) (reduce +)))

(comment
  (def test-data
    (parse-calories (slurp (io/resource "day1/input.example.txt"))))
  (max-calories test-data) ;; => 24000
  (top3 test-data) ;; 45000

  (def data (parse-calories (slurp (io/resource "day1/input.txt"))))
  (max-calories data) ;; => 75622
  (top3 data) ;; => 213159
  )

(defn -main
  [& args]
  (let [in (or (first args) System/in)
        data (slurp in)]
    (max-calories data)))
