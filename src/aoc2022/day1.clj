(ns aoc2022.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn max-calories [s]
  (->> (str/split-lines s)
       (partition-by #(= % ""))
       (filter #(not= % '("")))
       (map (fn [xs] (reduce + (map #(Integer/parseInt %) xs))))
       (reduce max)))

(comment
  (max-calories (slurp (io/resource "day1/input.example.txt"))) ;; => 24000
  (max-calories (slurp (io/resource "day1/input.txt"))) ;; => 75622
  )

(defn -main
  [& args]
  (let [in (or (first args) System/in)
        data (slurp in)]
    (max-calories data)))
