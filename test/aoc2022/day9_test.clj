(ns aoc2022.day9-test
  (:require [aoc2022.day9 :refer :all]
            [clojure.test :refer :all]))

(deftest test-get-bounds
  (is (= [[0 1] [2 3]] (get-bounds [(lp \H 1 2)] 1)))
  (is (= [[0 -1] [5 4]] (get-bounds [(lp \H 1 3) (lp \T 4 0)] 1))))

(deftest test-p-minus
  (is (= [1 2] (p-minus [2 5] [1 3]))))

(deftest test-touching
  (is (touching? [0 0] [0 0]))
  (is (touching? [0 0] [1 0]))
  (is (touching? [0 0] [0 1]))
  (is (touching? [0 0] [1 1]))
  (is (touching? [0 0] [-1 -1]))
  (is (touching? [-1 -1] [0 0]))
  (is (not (touching? [-1 -1] [1 1])))
  (is (not (touching? [-1 0] [1 1])))
  (is (not (touching? [-1 0] [1 0]))))

(deftest test-direction-to
  (is (= [0 0] (direction-to [0 0] [0 0])))

  (is (= [0 1] (direction-to [0 0] [0 1])))
  (is (= [0 1] (direction-to [0 0] [0 3])))
  (is (= [0 -1] (direction-to [0 0] [0 -1])))
  (is (= [0 -1] (direction-to [0 1] [0 -5])))

  (is (= [1 0] (direction-to [0 0] [1 0])))
  (is (= [1 0] (direction-to [0 0] [3 0])))
  (is (= [-1 0] (direction-to [0 0] [-1 0])))
  (is (= [-1 0] (direction-to [0 0] [-5 0])))

  (is (= [1 1] (direction-to [0 0] [1 1])))
  (is (= [1 1] (direction-to [0 0] [1 3])))
  (is (= [1 1] (direction-to [0 0] [3 1])))
  
  (is (= [1 -1] (direction-to [0 0] [1 -1])))
  (is (= [1 -1] (direction-to [0 0] [1 -5])))
  (is (= [1 -1] (direction-to [0 0] [5 -1])))

  (is (= [-1 1] (direction-to [0 0] [-1 1])))
  (is (= [-1 1] (direction-to [0 0] [-5 1])))
  (is (= [-1 1] (direction-to [0 0] [-1 5])))

  (is (= [-1 -1] (direction-to [0 0] [-1 -1])))
  (is (= [-1 -1] (direction-to [0 0] [-5 -1])))
  (is (= [-1 -1] (direction-to [0 0] [-1 -5]))))

(deftest test-parse-command
  (is (= [[0 1] 4] (parse-command "R 4"))))

(deftest test-state-to-lps
  (is (= [(lp \H 0 0) (lp \T 0 0)]
         (state-to-lps initial-short-state))))

(def test-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn apply-input [input state rope]
  (->> (parse-input input)
       (apply-commands state rope)
       last state-to-lps format-lps))

(deftest test-apply-command
  (is (= "...
.H.
.T.
..."
         (apply-input "R 4\nU 4" initial-short-state short-rope)))
  (is (= ".......
.....H.
.....1.
...432.
..5....
.6.....
......."
         (apply-input "R 4\nU 4" initial-long-state long-rope)))
  (is (= "......
..123.
..5...
.6....
......"
         (apply-input test-input initial-long-state long-rope))))


(deftest test-answer1
  (is (= 13 (answer1 (parse-input test-input)))))

(deftest test-answer2
  (is (= 36 (answer2 (parse-input test-input2)))))
