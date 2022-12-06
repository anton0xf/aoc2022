(ns aoc2022.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [clojure.test :refer :all]))

(defn search-marker [s marker-size]
  (->> s
       (partition marker-size 1)
       (keep-indexed
        (fn [idx part]
          (if (= marker-size (count (set part)))
            [(+ marker-size idx) (str/join part)])))
       first))

(defn search-start-of-packet-marker [s]
  (search-marker s 4))

(defn search-start-of-message-marker [s]
  (search-marker s 14))

(comment
  (def input (slurp (io/resource "day6/input.txt")))
  (search-start-of-packet-marker input) ;; => [1647 "dsjt"]
  (search-start-of-message-marker input) ;; => [2447 "lshwdrgjfbnptq"]
  )

