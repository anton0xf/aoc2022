(ns aoc2022.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]))

(def test-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse-input [s]
  (->> s str/split-lines (mapv vec)))

(defn search-on-map [c m]
  (->> m
       (keep-indexed
        (fn [i line]
          (if-let [j (->> line
                          (keep-indexed (fn [j v] (if (= c v) j)))
                          first)]
            [i j])))
       first))

(defn set-on-map [c ij m]
  (update-in m ij (fn [_] c)))

(defn input-to-map [s]
  (let [m (parse-input s)
        start (search-on-map \S m)
        end (search-on-map \E m)]
    {:start start :end end
     :map (->> m
               (set-on-map \a start)
               (set-on-map \z end)
               (mapv (fn [line] (mapv #(- (int %) (int \a)) line))))}))

(def test-data (input-to-map test-input))

(def directions {">" [0 1], "<" [0 -1], "^" [-1 0], "v" [1 0]})
(defn p-plus [p q] (mapv + p q))
(defn p-minus [p q] (mapv - p q))

(defn map-size [m] [(count m) (count (first m))])

(defn in-bounds? [p size]
  (every? identity
          (map (fn [v lim] (<= 0 v (dec lim))) p size)))

(defn neighbours [p size]
  (->> (vals directions)
       (map #(p-plus p %))
       (filter #(in-bounds? % size))))

(defn available-move? [p q m]
  (>= 1 (- (get-in m q)
           (get-in m p))))

(defn available-neigbours
  ([p m] (available-neigbours p m (map-size m)))
  ([p m size]
   (->> (neighbours p size)
        (filter #(available-move? p % m)))))

(defn get-direction [p q]
  (let [d (p-minus q p)]
    (->> directions (filter #(= d (second %))) first first)))

(defn get-path [p prevs]
  (loop [p p, path (list [p \E])]
    (if-let [prev (prevs p)]
      (recur prev (conj path [prev (get-direction prev p)]))
      path)))

(defn bfs [q neighbours-f done-p]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY q)
         visited #{}
         prevs {}]
    ;; (println "bfs step" "q" (seq q) "visited" visited "prevs" prevs)
    (if (seq q)
      (let [p (peek q)]
        (cond (done-p p) (get-path p prevs)
              (visited p) (recur (pop q) visited prevs)
              :else
              (let [ns (->> (neighbours-f p) (filter #(not (visited %))))]
                (recur (into (pop q) ns)
                       (conj visited p)
                       (into prevs (map (fn [np] [np p]) ns)))))))))

(defn bfs-on-map [{start :start end :end m :map}]
  (let [size (map-size m)]
    (bfs [start]
         #(available-neigbours % m size)
         #(= % end))))

(defn format-path [size path]
  (let [ps (into {} path)]
    (str/join "\n" (for [i (range (first size))]
                     (str/join (for [j (range (second size))]
                                 (get ps [i j] ".")))))))

(defn answer1 [data]
  (->> (bfs-on-map data) count dec))

(comment
  (->> test-input parse-input (search-on-map \E)) ;; => [2 5]
  (->> test-input input-to-map)
  ;; => {:start [0 0],
  ;;     :end [2 5],
  ;;     :map
  ;;     [[0 0 1 16 15 14 13 12]
  ;;      [0 1 2 17 24 23 23 11]
  ;;      [0 2 2 18 25 25 23 10]
  ;;      [0 2 2 19 20 21 22 9]
  ;;      [0 1 3 4 5 6 7 8]]}

  (let [{start :start end :end m :map} test-data
        size (map-size m)]
    (available-neigbours [4 0] m size))

  (->> (bfs-on-map test-data)
       (format-path (map-size (:map test-data)))
       println)
  (answer1 test-data) ;; => 31

  (def data (input-to-map (slurp (io/resource "day12/input.txt"))))
  (->> (bfs-on-map data)
       (format-path (map-size (:map data)))
       println)
  (answer1 data) ;; => 350
  )
