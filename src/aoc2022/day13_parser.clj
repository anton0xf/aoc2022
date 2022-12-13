(ns aoc2022.day13-parser
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(def test-input (-> (io/resource "day13/test.txt") slurp))

(defn is-digit? [c]
  (apply <= (map int [\0 c \9])))

(defn parse-char [pred]
  (fn [cs]
    (if (and (seq cs) (pred (first cs)))
      [(first cs) (rest cs)]
      [nil cs])))

(def parse-digit (parse-char is-digit?))

(defn success [res] (fn [cs] [res cs]))

(defn p-or [& ps]
  (fn [cs] (->> ps (map #(% cs)) (filter #(first %)) first)))

(defn p-and [f & ps]
  (fn [orig-cs]
    (loop [cs orig-cs, ps ps, res []]
      (if (empty? ps) [(f res) cs]
          (let [[r cs] ((first ps) cs)]
            (if (not r) [nil orig-cs]
                (recur cs (rest ps) (conj res r))))))))

(defn p-all [f p]
  (fn [cs]
    (loop [cs cs, res []]
      (let [[r cs] (p cs)]
        (if r (recur cs (conj res r))
            [(f res) cs])))))

(def parse-unsigned-int
  (p-all (fn [ds]
           (if (empty? ds) nil
               (->> ds
                    (map #(- (int %) (int \0)))
                    (reduce #(+ (* 10 %1) %2)))))
         parse-digit))

(def skip-spaces
  (p-all #(reduce conj [] %)
         (parse-char #(Character/isSpace %))))

(comment
  (parse-digit "1") ;; => [\1 ()]
  (parse-digit "")  ;; => [nil ""]
  (parse-digit "a") ;; => [nil "a"]

  ((p-all #(reduce str %) parse-digit) "12a") ;; => ["12" (\a)]
  (parse-unsigned-int (seq "12"))             ;; => [12 ()]
  (parse-unsigned-int (seq ""))               ;; => [nil nil]
  (parse-unsigned-int (seq "12a"))            ;; => [12 (\a)]

  ((parse-char #(Character/isSpace %)) "  2") ;; => [\space (\space \2)]
  ((p-all #(reduce conj [] %)
          (parse-char #(Character/isSpace %)))
   "  2") ;; => [[\space \space] (\2)]
  (skip-spaces "  a") ;; => [[\space \space] (\a)]
  )

(def parse-signed-int
  (p-or (p-and #(- (second %)) (parse-char #(= % \-)) parse-unsigned-int)
        (p-and #(second %) (parse-char #(= % \+)) parse-unsigned-int)
        parse-unsigned-int))

(comment
  ((parse-char #(= % \-)) "-1") ;; => [\- (\1)]
  ((p-and vec (parse-char #(= % \-)) parse-unsigned-int) "-1") ;; => [\- 1]
  ((p-or (parse-char #(= % \-)) parse-digit) "-") ;; => [\- ()]
  ((p-or (parse-char #(= % \-)) parse-digit) "1") ;; => [\1 ()]
  (parse-signed-int "-43") ;; => [-43 ()]
  )

(defn parse-list [p]
  (p-and second
         (parse-char #(= % \[))
         (p-or
          (p-and (fn [[x xs]] (cons x xs))
                 p (p-or (p-all seq (p-and last (parse-char #(= % \,)) skip-spaces p))
                         (success '())))
          (success '()))
         (parse-char #(= % \]))))

(defn parse-recur-int-list [cs]
  ((parse-list (p-or parse-signed-int parse-recur-int-list)) cs))

(comment
  ((p-and second (parse-char #(= % \,)) parse-unsigned-int skip-spaces) "") ;; => [nil ""]
  ((parse-char #(= % \[)) "[1]") ;; => [\[ (\1 \])]
  (parse-unsigned-int "1]")      ;; => [1 (\])]
  ((p-all seq (p-and second (parse-char #(= % \,))
                     parse-unsigned-int skip-spaces)) "]") ;; => [nil "]"]
  ((p-or (p-all seq (p-and second (parse-char #(= % \,))
                           parse-unsigned-int skip-spaces))
         (success '()))
   "]")                                         ;; => [() "]"]

  ((parse-list parse-unsigned-int) "[]")        ;; => [() ()]
  ((parse-list parse-unsigned-int) "[1]")       ;; => [(1) ()]
  ((parse-list parse-unsigned-int) "[1, 2]")    ;; => [(1 2) ()]
  ((parse-list parse-unsigned-int) "[1, 2, 3]") ;; => [(1 2 3) ()]

  (parse-recur-int-list "[]")   ;; => [() ()]
  (parse-recur-int-list "[1]")  ;; => [(1) ()]
  (parse-recur-int-list "[[]]") ;; => [(()) ()]
  (parse-recur-int-list "[[], 1]") ;; => [(() 1) ()]
  (parse-recur-int-list "[[1, [2, []], 3], [[[]]], [[4]]]") ;; => [((1 (2 ()) 3) ((())) ((4))) ()]

  (let [lines (->> (str/split-lines test-input) (filter #(not (str/blank? %))))]
    (= (map read-string lines)
       (map #(first (parse-recur-int-list %)) lines)))
  ;; => true
  )
