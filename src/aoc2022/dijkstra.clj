(ns aoc2022.dijkstra
  (:require [clojure.set :as sets]))

(defprotocol Node
  "Node of weighted oriented graph"
  (neighbors [node] "get seq of neighbour [dist, node] pairs"))

(deftype )
