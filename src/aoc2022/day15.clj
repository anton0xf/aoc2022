(ns aoc2022.day15
  (:require [aoc2022.util :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]))

(defn parse-line [line]
  (match (str/split line #"[\s=,:]+")
         ["Sensor" "at" "x" sx "y" sy "closest" "beacon" "is" "at" "x" bx "y" by]
         (mapv (fn [p] (mapv #(Integer/parseInt %) p))
               [[sx sy] [bx by]])))

(defn parse-input [in]
  (->> (line-seq in)
       (map parse-line)))

(def test-data
  (with-open [in (io/reader (io/resource "day15/test.txt"))]
    (vec (parse-input in))))

(defn dist [a b]
  (abs (- a b)))


(defn md "Manhattan distance" [p0 p1]
  (->> (map dist p0 p1)
       (apply +)))

;; (md s [x y]) <= (md s b)
;; (dist sx x) + (dist sy y) <= (md s b) =: r
;; 0 <= (dist sx x) = |sx - x| <= (md s b) - (dist sy y) =: ry
;; -ry <= x - sx <= ry
;; sx - ry <= x <= <= sx + ry
(defn interval-at-y [y [sx sy :as s] [bx by :as b]]
  (let [r (md s b)
        ry (- r (dist sy y))]
    (if (<= 0 ry)
      [(- sx ry) (+ sx ry)])))

(defn intervals-at-y [y data]
  (keep #(apply interval-at-y y %) data))

(defn in-interval? [x [l r]] (<= l x r))

;; inefficient reference implementation
(defn merge-intervals-count0 [intervals]
  (let [ls (map first intervals)
        rs (map second intervals)
        l-min (reduce min ls)
        r-max (reduce max rs)]
    (->> (range l-min (inc r-max))
         (filter (fn [x] (some #(in-interval? x %) intervals)))
         count)))

(defn merge-intervals [is]
  (let [ls (group-by first is)
        rs (group-by second is)
        xs (into (sorted-set) (concat (keys ls) (keys rs)))]
    (loop [xs xs, oc 0, l nil, res []]
      (if (empty? xs) res
          (let [x (first xs), xs (rest xs)
                l? (seq (get ls x))
                r? (seq (get rs x))
                oc (-> oc
                       (+ (count (get ls x [])))
                       (- (count (get rs x []))))
                l (if l? (or l x) l)]
            (cond (and r? (zero? oc)) (recur xs oc nil (conj res [l x]))
                  r? (recur xs oc l res)
                  :else (recur xs oc l res)))))))

(defn merge-intervals-count [is]
  (->> (merge-intervals is)
       (map (fn [[l r]] (inc (- r l))))
       (reduce +)))

(defn answer1 [y data]
  (let [is (intervals-at-y y data)
        n (merge-intervals-count is)
        bs (map second data)
        bn (->> bs (filter #(= y (second %))) set count)]
    (- n bn)))

;; part2
(defn is-complement [[l r] is]
  (loop [l l
         is (merge-intervals is)
         res []]
    ;; (println "l:" l "is:" is "res:" res)
    (if (empty? is) (conj res [l r])
        (let [[il ir] (first is), is (rest is)]
          (cond (< ir l) (recur l is res)
                (< r il) (conj res [l r])
                (<= il l r ir) []
                (<= il l) (do (assert (and (<= l ir) (< ir r)))
                              (recur (inc ir) is res))
                (<= r ir) (do (assert (and (< l il) (<= il r)))
                              (conj res [l (dec il)]))
                :else (do (assert (and (< l il) (< ir r)))
                          (recur (inc ir) is (conj res [l (dec il)]))))))))

(defn search-possible-locations [[[x-min y-min] [x-max y-max]] data]
  (for [y (range y-min (inc y-max))
        :let [cis (->> (intervals-at-y y data)
                       (is-complement [x-min x-max]))]
        :when (seq cis)]
    [y cis]))

(defn answer2 [bs data]
  (match (search-possible-locations bs data)
         ([[y [[x xr]]]] :seq)
         (do (assert (= x xr))
             [x y (+ y (* 4000000 x))])))

(comment
  (first test-data)
  ;; => [[2 18] [-2 15]]

  (->> test-data (intervals-at-y 10))
  ;; => ([12 12] [2 14] [2 2] [-2 2] [16 24] [14 18])

  (->> test-data (intervals-at-y 10) merge-intervals)
  ;; => [[-2 24]]

  (def data
    (with-open [in (io/reader (io/resource "day15/input.txt"))]
      (vec (parse-input in))))
  (answer1 2000000 data) ;; => 5688618

  ;; part2
  
  ;; In the example the x and y coordinates can each be at most 20
  (search-possible-locations [[0 0] [20 20]] test-data) ;; => ([11 [[14 14]]])

  (answer2 [[0 0] [20 20]] test-data) ;; => [14 11 56000011]
  
  ;; the distress beacon must have x and y coordinates each no lower than 0 and no larger than 4_000_000
  (time (answer2 [[0 0] [4000000 4000000]] data))
  ;; "Elapsed time: 290444.516162 msecs" :(
  ;; => [3156345 3204261 12625383204261]

  (time (doall (search-possible-locations [[0 0] [4000000 40000]] data)))
  ;; => ()
  ;; "Elapsed time: 2208.33445 msecs"


  )

