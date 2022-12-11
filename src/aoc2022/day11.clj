(ns aoc2022.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(defn split-to-monkeys [s]
  (->> (str/split-lines s)
       (partition-by str/blank?)
       (filter (fn [group] (every? #(not (str/blank? %)) group)))))

(defn split-line-to-words [line] (str/split line #"[\s:,]+"))

(defn parse-monkey-line [line]
  (match (split-line-to-words line)
         ["Monkey" n] [:num (Integer/parseInt n)]
         ["" "Starting" "items" & items] [:items (map #(Integer/parseInt %) items)]
         ["" "Operation" "new" "=" "old" op arg] [:operation [op (Integer/parseInt arg)]]
         ["" "Test" "divisible" "by" arg] [:test [:divisible-by (Integer/parseInt arg)]]
         ["" "If" test-result "throw" "to" "monkey" n] [[:if (Boolean/parseBoolean test-result)]
                                                        (Integer/parseInt n)]
         :else [:unexpected line]))

(defn parse-monkey [lines]
  (->> (map parse-monkey-line lines)
       (into {})))

(defn parse-input [s]
  (->> (split-to-monkeys s)
       (map parse-monkey)))

(def test-data
  (-> (io/resource "day11/test.txt")
      slurp parse-input))

(comment
  
  )

