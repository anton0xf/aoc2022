(ns aoc2022.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(def test-input "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse-point [s]
  (->> (str/split s #",")
       (mapv #(Integer/parseInt %))))

(defn parse-line [line]
  (->> (str/split line #"\s*->\s*")
       (map parse-point)))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-line)))

(def test-paths (parse-input test-input))



