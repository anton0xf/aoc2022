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
    ;; (println "ls:" ls "rs:" rs "xs:" xs)
    (loop [xs xs, oc 0, l nil, res []]
      ;; (println "xs:" xs "oc:" oc "l:" l "res:" res)
      (assert (<= 0 oc))
      (assert (or (and (pos? oc) l)
                  (and (zero? oc) (nil? l))))
      (if (empty? xs)
        (do (assert (nil? l) (format "l: %d" l))
            (assert (zero? oc))
            res)
        (let [x (first xs), xs (rest xs)
              l? (seq (get ls x))
              r? (seq (get rs x))
              oc (-> oc
                     (+ (count (get ls x [])))
                     (- (count (get rs x []))))
              l (if l? (or l x) l)]
          (assert (<= 0 oc))
          (cond (and r? (zero? oc)) (recur xs oc nil (conj res [l x]))
                r? (do (assert (pos? oc))
                       (recur xs oc l res))
                :else (do (assert (seq (get ls x)) (format "x: %s, ls: %s" x ls))
                          (recur xs oc l res))))))))

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
  )

