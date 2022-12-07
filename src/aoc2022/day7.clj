(ns aoc2022.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(def root "/")
(def initial-state {:path []
                    :tree {root {:type :dir
                                 :children {}}}})
(comment
  {:type :dir
   :children {"a" {:type :dir
                   :children {}}
              "b.txt" {:type :file
                       :size 12}}})

(def command-prefix "$ ")

(defn command? [s]
  (str/starts-with? s command-prefix))

(defn path-to-ks [path]
  (into [:tree] (interpose :children path)))

(defn cd [state name]
  (update
   state :path
   (fn [path]
     (match [name]
            [".."] (let [path (vec (drop-last path))]
                     (if (seq path) path [root]))
            ["/"] [root]
            :else (conj path name)))))

(defn parse-content-line [s]
  (let [[type-or-size name] (str/split s #"\s")]
    [name (if (= "dir" type-or-size)
            {:type :dir :children {}}
            {:type :file :size (Integer/parseInt type-or-size)})]))

(defn ls [state content]
  (update-in
   state (path-to-ks (:path state))
   (fn [dir]
     (assert (= :dir (:type dir))
             (format "dir: %s, path: %s" dir (path-to-ks (:path state))))
     (update dir :children
             #(into % (map parse-content-line content))))))

(defn apply-next-command [state lines]
  (assert (seq lines))
  (assert (command? (first lines)))
  (match [(-> (first lines)
              (subs (.length command-prefix))
              (str/split #"\s"))]
         [["cd" name]] {:state (cd state name)
                        :lines (rest lines)}
         [["ls"]] (let [[content rest-lines] (split-with #(not (command? %)) (rest lines))]
                    {:state (ls state content)
                     :lines rest-lines})))

(defn scan-history-step [{state :state lines :lines :as arg}]
  (if (seq lines)
    (apply-next-command state lines)
    arg))

(defn scan-history [state lines]
  (->> {:state state :lines lines}
       (iterate scan-history-step)
       (drop-while #(seq (:lines %)))
       first :state))

(defn traverse-dir [dir f]
  (let [named-children
        (map (fn [[name val]]
               [name (match (:type val)
                            :dir (traverse-dir val f)
                            :file val)])
             (:children dir))]
    (f dir named-children)))

(defn calc-size [dir]
  (traverse-dir
   dir
   (fn [d kv]
     {:type :dir
      :size (reduce + (map (comp :size second) kv))
      :children (->> kv
                     (filter #(= :dir (:type (second %))))
                     (into {}))})))

(comment
  (map identity {"a" 1 "b" 2}) ;; => (["a" 1] ["b" 2])

  (def test-data (str/split-lines (slurp (io/resource "day7/test-input.txt"))))
  (scan-history initial-state test-data)
  ;; => {:path ["/" "d"],
  ;;     :tree
  ;;     {"/"
  ;;      {:type :dir,
  ;;       :children
  ;;       {"a"
  ;;        {:type :dir,
  ;;         :children
  ;;         {"e" {:type :dir, :children {"i" {:type :file, :size 584}}},
  ;;          "f" {:type :file, :size 29116},
  ;;          "g" {:type :file, :size 2557},
  ;;          "h.lst" {:type :file, :size 62596}}},
  ;;        "b.txt" {:type :file, :size 14848514},
  ;;        "c.dat" {:type :file, :size 8504156},
  ;;        "d"
  ;;        {:type :dir,
  ;;         :children
  ;;         {"j" {:type :file, :size 4060174},
  ;;          "d.log" {:type :file, :size 8033020},
  ;;          "d.ext" {:type :file, :size 5626152},
  ;;          "k" {:type :file, :size 7214296}}}}}}}

  (calc-size (-> (scan-history initial-state test-data) :tree (get root)))
  ;; => {:type :dir,
  ;;     :size 48381165,
  ;;     :children
  ;;     {"a"
  ;;      {:type :dir,
  ;;       :size 94853,
  ;;       :children {"e" {:type :dir, :size 584, :children {}}}},
  ;;      "d" {:type :dir, :size 24933642, :children {}}}}

  
  )

