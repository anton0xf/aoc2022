(ns aoc2022.day7-test
  (:require  [aoc2022.day7 :refer :all]
             [clojure.test :refer :all]))

(deftest test-apply-next-command
  (is (= {:state {:path ["/"],
                  :tree [{:type :dir, :name "/"}]},
          :lines ["rest"]}
         (apply-next-command initial-state ["$ cd /" "rest"])))
  (is (= {:state {:path ["/"],
                  :tree [{:type :dir, :name "/"
                          :children [{:type :dir, :name "a"},
                                     {:type :file, :name "b.txt", :size 12}]}]},
          :lines ["$ rest"]}
         (apply-next-command (cd initial-state "/")
                             ["$ ls" "dir a" "12 b.txt" "$ rest"])))
  (is (= {:state {:path ["/"],
                  :tree [{:type :dir, :name "/",
                          :children [{:type :dir, :name "a"},
                                     {:type :file, :name "b.txt", :size 12}]}]},
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
  (is (= {:type :dir, :name "/", :size 584}
         (calc-size {:type :dir, :name "/"
                     :children [{:type :file, :name "i", :size 584}]})))
  (is (= {:type :dir, :name "/" :size 29700,
          :children [{:type :dir, :name "e", :size 584}]}
         (calc-size {:type :dir, :name "/"
                     :children [{:type :dir, :name "e", :children [{:type :file, :name "i", :size 584}]},
                                {:type :file, :name "f", :size 29116}]}))))

(deftest test-flat-tree
  (is (= [{:type :dir, :name "e", :size 584}]
         (flat-dir {:type :dir, :name "e", :size 584})))
  (is (= [{:type :dir, :name "a", :size 29700}
          {:type :dir, :name "e", :size 584}]
         (flat-dir {:type :dir, :name "a", :size 29700,
                    :children [{:type :dir, :name "e", :size 584}]})))
  (is (= [{:type :dir, :size 29700, :name "a"}
          {:type :dir, :size 584, :name "e"}]
         (flat-dir {:type :dir, :name "a", :size 29700,
                    :children [{:type :dir, :name "e", :size 584}]}))))
