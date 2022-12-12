(ns aoc2022.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(defn split-to-monkeys [s]
  (->> (str/split-lines s)
       (partition-by str/blank?)
       (filter (fn [group] (every? #(not (str/blank? %)) group)))))

(defn split-line-to-words [line] (str/split line #"[\s:,]+"))

(defn parse-monkey-line [line]
  (match (split-line-to-words line)
         ["Monkey" n] [:id (Integer/parseInt n)]
         ["" "Starting" "items" & items] [:items (map #(Integer/parseInt %) items)]
         ["" "Operation" "new" "=" "old" op arg] [:operation [op (if (= "old" arg) :old
                                                                     (Integer/parseInt arg))]]
         ["" "Test" "divisible" "by" arg] [:test [:divisible-by (Integer/parseInt arg)]]
         ["" "If" test-result "throw" "to" "monkey" n] [[:if (Boolean/parseBoolean test-result)]
                                                        (Integer/parseInt n)]
         :else [:unexpected line]))

(defn parse-monkey [lines]
  (->> (map parse-monkey-line lines)
       (into {})))

(defn parse-input [s]
  (let [monkeys (->> (split-to-monkeys s)
                     (map parse-monkey))
        ids (map :id monkeys)
        state (zipmap ids monkeys)]
    [ids state]))

(def test-data
  (-> (io/resource "day11/test.txt")
      slurp parse-input))

(defn inspect-item [item monkey]
  (println "inspect-item" [item monkey])
  (match (:operation monkey)
         ["+" n] (+ item n)
         ["*" :old] (* item item)
         ["*" n] (* item n)))

(defn test-item [item monkey]
  (match (:test monkey)
         [:divisible-by n] (zero? (rem item n))))

(defn monkey-turn
  "process one monkey's item"
  [item monkey state]
  (let [inspected-level (inspect-item item monkey)
        new-level (quot inspected-level 3)
        test-res (test-item new-level monkey)
        new-id (get monkey [:if test-res])]
    (println "monkey-turn"
             "inspected-level" inspected-level
             "new-level" new-level
             "test-res" test-res
             "new-id" new-id)
    (update-in state [new-id :items]
               (fn [items] (concat items [new-level])))))

(defn monkey-round
  "process all monkey's items"
  [id state] ;; -> state
  (let [monkey (get state id)]
    (loop [monkey monkey
           state state]
      (let [items (:items monkey)]
        (if (empty? items)
          (assoc state id monkey)
          (recur (assoc monkey :items (rest items))
                 (monkey-turn (first items) monkey state)))))))

(defn full-round [ids state]
  (if (empty? ids) state
      (recur (rest ids)
             (monkey-round (first ids) state))))

(defn rounds [ids state]
  (iterate #(full-round ids %) state))

(comment
  (-> test-data second (get 0))
  ;; => {:id 0,
  ;;     :items (79 98),
  ;;     :operation ["*" 19],
  ;;     :test [:divisible-by 23],
  ;;     [:if true] 2,
  ;;     [:if false] 3}

  (->> test-data second
       (monkey-round 0))

  (apply full-round test-data)
  ;; => {0
  ;;     {:id 0,
  ;;      :items (20 23 27 26),
  ;;      :operation ["*" 19],
  ;;      :test [:divisible-by 23],
  ;;      [:if true] 2,
  ;;      [:if false] 3},
  ;;     1
  ;;     {:id 1,
  ;;      :items (2080 25 167 207 401 1046),
  ;;      :operation ["+" 6],
  ;;      :test [:divisible-by 19],
  ;;      [:if true] 2,
  ;;      [:if false] 0},
  ;;     2
  ;;     {:id 2,
  ;;      :items (),
  ;;      :operation ["*" :old],
  ;;      :test [:divisible-by 13],
  ;;      [:if true] 1,
  ;;      [:if false] 3},
  ;;     3
  ;;     {:id 3,
  ;;      :items (),
  ;;      :operation ["+" 3],
  ;;      :test [:divisible-by 17],
  ;;      [:if true] 0,
  ;;      [:if false] 1}}
  )
