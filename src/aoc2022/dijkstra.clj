(ns aoc2022.dijkstra
  (:require [clojure.set :as sets]))
;; https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

(defn neighbours
  "return seq of pairs [distance node] of neighbours of node v in graph g"
  [v g] (get g v))

(defn symmetric? [g]
  (every? (fn [[u ns]]
            (every? (fn [[d v]]
                      (= d (->> (get g v)
                                (filter #(= u (second %)))
                                first first)))
                    ns))
          g))

(defn upd-dist [u dist g]
  (->> (get g u)
       (map (fn [[d v]] {v (+ d (dist u))}))
       (into {})
       (merge-with min dist)))


(defn step [vis dist g] ;; -> [vis dist]
  (let [u (->> (keys dist)
               (filter #(not (vis %)))
               first)
        vis (conj vis u)
        dist (upd-dist u dist g)]
    [vis dist]))

(defn visit-all [u g]
  (let [N (count (keys g))]
    (->> [#{} {u 0}]
         (iterate (fn [[vis dist]] (step vis dist g)))
         (take-while (fn [[vis _]] (<= (count vis) N))))))

(defn dijkstra [u g])
(comment
  (def g1 {1 [[7 2] [9 3] [14 6]]
           2 [[7 1] [10 3] [15 4]]
           3 [[9 1] [10 2] [11 4] [2 6]]
           4 [[15 2] [11 3] [6 5]]
           5 [[6 4] [9 6]]
           6 [[14 1] [2 3] [9 5]]})
  (symmetric? g1) ;; => true

  (upd-dist 1 {1 0} g1)
  ;; => {1 0, 2 7, 3 9, 6 14}
  (upd-dist 2 {1 0, 2 7, 3 9, 6 14} g1)
  ;; => {1 0, 2 7, 3 9, 6 14, 4 22}
  
  (step #{} {1 0} g1)
  ;; => [#{1} {1 0, 2 7, 3 9, 6 14}]
  (step #{1} {1 0, 2 7, 3 9, 6 14} g1)
  ;; => [#{1 2} {1 0, 2 7, 3 9, 6 14, 4 22}]
  (step #{1 2} {1 0, 2 7, 3 9, 6 14, 4 22} g1)
  ;; => [#{1 3 2} {1 0, 2 7, 3 9, 6 11, 4 20}]

  (visit-all 1 g1)
  ;; => ([#{} {1 0}]
  ;;     [#{1} {1 0, 2 7, 3 9, 6 14}]
  ;;     [#{1 2} {1 0, 2 7, 3 9, 6 14, 4 22}]
  ;;     [#{1 3 2} {1 0, 2 7, 3 9, 6 11, 4 20}]
  ;;     [#{1 6 3 2} {1 0, 2 7, 3 9, 6 11, 4 20, 5 20}]
  ;;     [#{1 4 6 3 2} {1 0, 2 7, 3 9, 6 11, 4 20, 5 20}]
  ;;     [#{1 4 6 3 2 5} {1 0, 2 7, 3 9, 6 11, 4 20, 5 20}])
  )
