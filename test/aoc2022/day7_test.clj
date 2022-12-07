(ns aoc2022.day7-test
  (:require  [aoc2022.day7 :refer :all]
             [clojure.test :refer :all]))

(deftest test-apply-next-command
  (is (= {:state {:path ["/"],
                  :tree {"/" {:type :dir, :children {}}}},
          :lines ["rest"]}
         (apply-next-command initial-state ["$ cd /" "rest"])))
  (is (= {:state {:path ["/"],
                  :tree {"/" {:type :dir,
                              :children {"a" {:type :dir, :children {}},
                                         "b.txt" {:type :file, :size 12}}}}},
          :lines ["$ rest"]}
         (apply-next-command (cd initial-state "/")
                             ["$ ls" "dir a" "12 b.txt" "$ rest"])))
  (is (= {:state {:path ["/"],
                  :tree {"/" {:type :dir,
                              :children {"a" {:type :dir, :children {}},
                                         "b.txt" {:type :file, :size 12}}}}},
          :lines []}
         (apply-next-command (cd initial-state "/")
                             ["$ ls" "dir a" "12 b.txt"]))))

(defn path-after-cds [names]
  (:path (reduce (fn [state name] (cd state name))
                 initial-state names)))

(deftest test-cd
  (is (= ["/"] (path-after-cds ["/"])))
  (is (= ["/"] (path-after-cds ["/" ".."])))
  (is (= ["/" "a"] (path-after-cds ["/" "a"])))
  (is (= ["/"] (path-after-cds ["/" "a" ".."])))
  (is (= ["/" "b"] (path-after-cds ["/" "a" ".." "b"])))
  (is (= ["/" "a" "b"] (path-after-cds ["/" "a" "b"])))
  (is (= ["/" "a"] (path-after-cds ["/" "a" "b" ".."])))
  (is (= ["/"] (path-after-cds ["/" "a" "b" "/"]))))

(deftest test-calc-size
  (is (= {:type :dir, :size 584, :children {}}
         (calc-size {:type :dir,
                     :children {"i" {:type :file, :size 584}}})))
  (is (= {:type :dir,
          :size 29700,
          :children {"e" {:type :dir, :size 584, :children {}}}}
         (calc-size {:type :dir,
                     :children
                     {"e" {:type :dir, :children {"i" {:type :file, :size 584}}},
                      "f" {:type :file, :size 29116}}}))))
