(ns aoc2022.day13-parser-test
  (:require [aoc2022.day13-parser :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(deftest test-parse-char
  (is (= [\- '(\1)] ((parse-char #(= % \-)) "-1")))
  (is (= [\[ '(\1 \])] ((parse-char #(= % \[)) "[1]"))))

(deftest test-parse-digit
  (is (= [\1 ()] (parse-digit "1")))
  (is (= [nil ""] (parse-digit "")))  
  (is (= [nil "a"] (parse-digit "a"))))

(deftest test-p-all
  (is (= ["12" '(\a)] ((p-all #(reduce str %) parse-digit) "12a")))
  (is (= [[\space \space] '(\2)]
         ((p-all #(reduce conj [] %)
                 (parse-char #(Character/isSpace %)))
          "  2")))
  (is (= [nil "]"]
         ((p-all seq (p-and second (parse-char #(= % \,))
                            parse-unsigned-int skip-spaces)) "]"))))

(deftest test-p-and
  (is (= [[\- 1] '()]
         ((p-and vec (parse-char #(= % \-)) parse-unsigned-int) "-1")))
  (is (= [nil ""]
         ((p-and second (parse-char #(= % \,)) parse-unsigned-int skip-spaces) ""))))

(deftest test-p-or
  (is (= [\- '()] ((p-or (parse-char #(= % \-)) parse-digit) "-")))
  (is (= [\1 '()] ((p-or (parse-char #(= % \-)) parse-digit) "1")))
  (is (= [() "]"]
         ((p-or (p-all seq (p-and second (parse-char #(= % \,))
                                  parse-unsigned-int skip-spaces))
                (success '()))
          "]"))))

(deftest test-parse-unsigned-int
  (is (= [12 '()] (parse-unsigned-int (seq "12"))))
  (is (= [nil nil] (parse-unsigned-int (seq ""))))
  (is (= [12 '(\a)] (parse-unsigned-int (seq "12a"))))
  (is (= [1 '(\])] (parse-unsigned-int "1]"))))

(deftest test-parse-int
  (is (= [-43 '()] (parse-int "-43"))))

(deftest test-skip-spaces
  (is (= [[\space \tab \space] '()] (skip-spaces " \t ")))
  (is (= [[\space \space] '(\a)] (skip-spaces "  a"))))

(deftest test-parse-list
  (is (= ['() '()] ((parse-list parse-unsigned-int) "[]")))
  (is (= ['(1) '()] ((parse-list parse-unsigned-int) "[1]")))
  (is (= ['(1 2) '()] ((parse-list parse-unsigned-int) "[1, 2]")))
  (is (= ['(1 2 3) '()] ((parse-list parse-unsigned-int) "[1, 2, 3]"))))

(def test-input (-> (io/resource "day13/test.txt") slurp))
(def test-lines
  (->> (str/split-lines test-input)
       (filter #(not (str/blank? %)))))

(deftest test-parserecur-int-list
  (is (= ['() '()] (parse-recur-int-list "[]")))
  (is (= ['(1) '()] (parse-recur-int-list "[1]")))
  (is (= ['(()) '()] (parse-recur-int-list "[[]]")))
  (is (= ['(() 1) '()] (parse-recur-int-list "[[], 1]")))
  (is (= ['((1 (2 ()) 3) ((())) ((4))) '()]
         (parse-recur-int-list "[[1, [2, []], 3], [[[]]], [[4]]]")))

  (is (= (map read-string test-lines)
         (map #(first (parse-recur-int-list %)) test-lines))))
