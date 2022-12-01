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
  (def s (slurp (io/resource "day1/input.example.txt")))
  (max-calories s))

(defn -main
  [& args]
  (let [in (or (first args) System/in)
        data (slurp in)]
    (max-calories data)))
