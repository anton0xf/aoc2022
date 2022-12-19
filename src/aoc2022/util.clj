(ns aoc2022.util
  (:require [clojure.string :as str]))

(defn bounds [gap ps]
  [[(->> ps (map first) (reduce min) (#(- % gap)))
    (->> ps (map second) (reduce min) (#(- % gap)))]
   [(->> ps (map first) (reduce max) (+ gap))
    (->> ps (map second) (reduce max) (+ gap))]])

(defn in-bounds? [[x y] [[x-min y-min] [x-max y-max]]]
  (and (<= x-min x x-max)
       (<= y-min y y-max)))

(def p-zero [0 0])
(defn p-plus [p1 p2] (mapv + p1 p2))
(defn p-minus [p1 p2] (mapv - p1 p2))

(defn scalar-direction [d]
  (if (= 0 d) 0
      (/ d (abs d))))

(defn direction-to [p0 p1]
  (let [r (p-minus p1 p0)]
    (if (= p-zero r) p-zero
        (mapv scalar-direction r))))
