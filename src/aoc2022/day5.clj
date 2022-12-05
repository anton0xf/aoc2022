(ns aoc2022.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(defn parse-stacks-line [s]
  (map last (re-seq #"(?:(\s{3})|.(.).)\s?" s)))

(defn transpose [m]
  (apply mapv vector m))

(defn parse-stacks [s]
  (->> (str/split-lines s)
       (map parse-stacks-line)
       transpose
       (map pop)
       (map #(drop-while nil? %))
       vec))

(defn parse-command [s]
  (let [[count from to]
        (->> (str/split s #"\s")
             (drop 1) 
             (take-nth 2)
             (map #(Integer/parseInt %)))]
    {:count count :from (dec from) :to (dec to)}))

(defn parse-input [s]
  (let [[stacks-str commands-str] (str/split s #"\n\n")]
    {:stacks (parse-stacks stacks-str)
     :commands (map parse-command (str/split-lines commands-str))}))

(defn apply-command [stacks {count :count from :from to :to}]
  (let [crates (take count (nth stacks from))]
    (-> stacks
        (update from #(drop count %))
        (update to #(concat (reverse crates) %)))))

(defn apply-commands [{stacks :stacks commands :commands}]
  (loop [stacks stacks, commands commands]
    (if (seq commands)
      (recur (apply-command stacks (first commands))
             (rest commands))
      stacks)))

(defn get-top-of-stacks [stacks]
  (str/join (map first stacks)))

(comment
  (def test-input (slurp (io/resource "day5/test-input.txt")))

  (def stasks-str (first (str/split test-input #"\n\n")))
  (parse-stacks stasks-str)
  ;; => [("N" "Z") ("D" "C" "M") ("P")]

  (parse-command "move 1 from 2 to 3")
  ;; => {:count 1, :from 2, :to 3}

  (parse-input test-input)
  ;; => {:stacks [("N" "Z") ("D" "C" "M") ("P")],
  ;;     :commands
  ;;     ({:count 1, :from 1, :to 0}
  ;;      {:count 3, :from 0, :to 2}
  ;;      {:count 2, :from 1, :to 0}
  ;;      {:count 1, :from 0, :to 1})}

  (apply-command '[("N" "Z") ("D" "C" "M") ("P")]
                 {:count 2, :from 1, :to 0})
  ;; => [("D" "C" "N" "Z") ("M") ("P")]

  (get-top-of-stacks (apply-commands (parse-input test-input)))

  (def data (parse-input (slurp (io/resource "day5/input.txt"))))
  (get-top-of-stacks (apply-commands data)) ;; => "HNSNMTLHQ"
  )

