;; https://adventofcode.com/2022/day/2
(ns aoc2022.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; First column is what your opponent is going to play:
;; A for Rock, B for Paper, and C for Scissors

;; The second column is what you should play in response:
;; X for Rock, Y for Paper, and Z for Scissors

;; The score for a single round is the score for the shape you selected
;; (1 for Rock, 2 for Paper, and 3 for Scissors)

(def selection-score {"X" 1, "Y" 2, "Z" 3})

;; plus the score for the outcome of the round
;; (0 if you lost, 3 if the round was a draw, and 6 if you won).

(def outcomes {:lose 0, :draw 3, :win 6})

;; Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock
;; If both players choose the same shape, the round instead ends in a draw.

(defn win [x y]
  (match [x y]
         ["A" "Z"] :lose ["C" "X"] :win
         ["C" "Y"] :lose ["B" "Z"] :win
         ["B" "X"] :lose ["A" "Y"] :win
         [_ _] :draw))

(defn parse-input [s]
  (->> (str/split-lines test-guide-input)
       (map #(str/split % #" "))))

(defn score [[x y]]
  (+ (selection-score y)
     (outcomes (win x y))))

(comment
  (def test-guide-input "A Y\nB X\nC Z")
  (def test-guide (parse-input test-guide-input))
  test-guide ;; => (["A" "Y"] ["B" "X"] ["C" "Z"])
  (map (fn [[x y]]
         {:score (selection-score y)
          :outcome (outcomes (win x y))})
       test-guide)
  ;; => ({:score 2, :outcome 6} {:score 1, :outcome 0} {:score 3, :outcome 3})
  )
