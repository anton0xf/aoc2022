(ns aoc2022.day14
  (:require [aoc2022.util :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]))

(def test-input "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse-input [s]
  (for [line (str/split-lines s)]
    (for [point (str/split line #"\s*->\s*")]
      (mapv #(Integer/parseInt %)
            (str/split point #",")))))

(def test-paths (parse-input test-input))

(def sand-source [500 0])

(defn trace-segment [p0 p1]
  (let [r (direction-to p0 p1)]
    (loop [p p0, res []]
      (if (= p p1) (conj res p)
          (recur (p-plus p r) (conj res p))))))

(defn trace-path [path]
  (->> path (partition 2 1)
       (map #(apply trace-segment %))
       (reduce (fn [ps1 ps2] (concat ps1 (rest ps2))))))

(defn paths-to-map [sand-source paths]
  (let [bs (->> paths (reduce concat)
                (#(conj % sand-source))
                (bounds 1))]
    {:bounds bs
     :source sand-source
     :sand 0
     :ps (->> paths 
              (map trace-path) (reduce concat)
              (map #(vector % \#))
              (into {sand-source \+}))}))

(defn map-to-str [{ps :ps sand :sand
                   [[x-min y-min] [x-max y-max]] :bounds}]
  (str/join
   "\n"
   (cons (format "sand: %d" sand)
         (for [y (range y-min (inc y-max))]
           (str/join (for [x (range x-min (inc x-max))]
                       (str (get ps [x y] \.))))))))

(defn print-map [m]
  (println (map-to-str m))
  (newline))

(def sand-fall-directions [[0 1] [-1 1] [1 1]])

(defn sand-step [p {bs :bounds ps :ps}]
  (->> sand-fall-directions
       (map (fn [r] (p-plus p r)))
       (filter (fn [new-p] (nil? (ps new-p))))
       first))

(defn sand-fall [m] ; -> m
  (let [bs (:bounds m), bs1 (bounds 1 bs)
        trace (->> (:source m)
                   (iterate #(sand-step % m)) rest
                   (take-while (fn [p] (and p (in-bounds? p bs1)))))
        end-p (last trace)]
    (if (in-bounds? end-p bs)
      (-> m
          (assoc-in [:ps end-p] \o)
          (update :sand inc))
      (reduce #(assoc-in %1 [:ps %2] \~)
              (assoc m :overflow true)
              trace))))

(defn sand-overflow [m] ; -> m
  (->> (iterate sand-fall m)
       (drop-while #(not (:overflow %)))
       first))

(comment
  test-paths
  ;; => (([498 4] [498 6] [496 6])
  ;;     ([503 4] [502 4] [502 9] [494 9]))

  (->> test-ps (reduce concat) (bounds 1))
  ;; => [[493 3] [504 10]]

  (->> [[[0 0] [2 0]]] (paths-to-map [1 -2]))
  ;; => {:bounds [[-1 -1] [3 1]],
  ;;     :source [1 2],
  ;;     :ps {[1 2] \+, [0 0] \#, [1 0] \#, [2 0] \#}}

  (->> test-paths (paths-to-map sand-source) print-map)

  (->> [[[0 0] [2 0]]] (paths-to-map [1 -3])
       sand-overflow print-map)

  (->> test-paths (paths-to-map sand-source)
       sand-overflow print-map)

  (->> (io/resource "day14/input.txt") slurp
       parse-input
       (paths-to-map sand-source)
       print-map)
  
  (->> (io/resource "day14/input.txt") slurp
       parse-input
       (paths-to-map sand-source)
       sand-overflow print-map)
  ;; sand: 1078
  )
