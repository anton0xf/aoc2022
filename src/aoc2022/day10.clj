(ns aoc2022.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(defn parse-cmd [s]
  (let [[cmd arg] (str/split s #"\s")]
    (case cmd
      "noop" [cmd]
      "addx" [cmd (Integer/parseInt arg)])))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-cmd)
       (map-indexed (fn [id cmd] [id cmd]))))

(def test-input "noop
addx 3
addx -5")

(defn apply-cmds [x cmds]
  (loop [cycle 1, x x, cmds cmds, xs []]
    (if (empty? cmds) {:xs xs :next-x x}
        (let [[idx [cmd arg]] (first cmds)]
          (case cmd
            "noop" (recur (inc cycle) x (rest cmds)
                          (conj xs [cycle x]))
            "addx" (let [new-x (+ x arg)]
                     (recur (+ 2 cycle) new-x (rest cmds)
                            (into xs [[cycle x] [(inc cycle) x]]))))))))

(def test-data2 (parse-input (slurp (io/resource "day10/test-input2.txt"))))

(defn get-key-points [xs]
  (->> xs (drop 19) (take-nth 40)))

(defn answer1 [cmds]
  (->> cmds (apply-cmds 1)
       :xs get-key-points
       (map (partial apply *))
       (reduce +)))

;; part 2

(defn answer2 [cmds]
  (let [xs (->> cmds (apply-cmds 1) :xs vec)]
    (str/join "\n" (for [i (range 6)]
                     (apply str (for [j (range 40)
                                      :let [idx (+ j (* i 40))
                                            x (get xs idx)]]
                                  (if (>= 1 (abs (- (second x) j)))
                                    \# \.)))))))
(comment
  (->> (range) (map #(+ 20 (* 40 %))) (take 6))
  ;; => (20 60 100 140 180 220)

  (->> test-data2 (apply-cmds 1)
       :xs get-key-points
       (map (partial apply *)))
  ;; => (420 1140 1800 2940 2880 3960)

  (def data (parse-input (slurp (io/resource "day10/input.txt"))))
  (answer1 data) ;; => 13180

  ;; part 2
  (println (answer2 test-data2))
  ;; ##..##..##..##..##..##..##..##..##..##..
  ;; ###...###...###...###...###...###...###.
  ;; ####....####....####....####....####....
  ;; #####.....#####.....#####.....#####.....
  ;; ######......######......######......####
  ;; #######.......#######.......#######.....
  
  (println (answer2 data))
  ;; ####.####.####..##..#..#...##..##..###..
  ;; #.......#.#....#..#.#..#....#.#..#.#..#.
  ;; ###....#..###..#....####....#.#..#.###..
  ;; #.....#...#....#....#..#....#.####.#..#.
  ;; #....#....#....#..#.#..#.#..#.#..#.#..#.
  ;; ####.####.#.....##..#..#..##..#..#.###..

  ;; EZFCHJAB
  )
