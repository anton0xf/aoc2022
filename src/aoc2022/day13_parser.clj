(ns aoc2022.day13-parser
  (:require [clojure.string :as str]))

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

(def parse-int
  (p-or (p-and #(- (second %)) (parse-char #(= % \-)) parse-unsigned-int)
        (p-and #(second %) (parse-char #(= % \+)) parse-unsigned-int)
        parse-unsigned-int))

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
  ((parse-list (p-or parse-int parse-recur-int-list)) cs))
