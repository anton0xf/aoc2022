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

(defn traverse-dir [name dir file-fn dir-fn]
  (->> (:children dir)
       (map (fn [[name val]]
              (match (:type val)
                     :dir (traverse-dir name val file-fn dir-fn)
                     :file (file-fn name val))))
       (dir-fn name dir)))

(defn calc-size [name dir]
  (second
   (traverse-dir
    name dir
    (fn [name file] [name file])
    (fn [name dir acc]
      [name
       {:type :dir
        :size (reduce + (map (comp :size second) acc))
        :children (->> acc
                       (filter #(= :dir (:type (second %))))
                       (into {}))}]))))

(defn flat-dir [name dir]
  (traverse-dir
   name dir
   (fn [name file] (assoc file :name name))
   (fn [name dir acc]
     (cons (-> dir (dissoc :children) (assoc :name name))
           (reduce concat acc)))))

(defn flat-tree [tree]
  (->> tree
       (map (fn [[name dir]] (flat-dir name dir)))
       (reduce concat)))

(defn answer1 [lines]
  (->> (scan-history initial-state lines)
       :tree (#(get % root))
       (calc-size root)
       (flat-dir root)
       (map :size)
       (filter #(>= 100000 %))
       (reduce +)))

(defn answer2 [lines]
  (let [root-with-size
        (->> (scan-history initial-state lines)
             :tree (#(get % root))
             (calc-size root))
        used (:size root-with-size)
        max-used (- 70000000 30000000)
        remove-at-least (- used max-used)]
    (->> root-with-size
         (flat-dir root)
         (map :size)
         (filter #(<= remove-at-least %))
         (reduce min))))

(comment
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

  (-> (scan-history initial-state test-data)
      :tree (get root)
      (#(calc-size root %)))
  ;; => {:type :dir,
  ;;     :size 48381165,
  ;;     :children
  ;;     {"a"
  ;;      {:type :dir,
  ;;       :size 94853,
  ;;       :children {"e" {:type :dir, :size 584, :children {}}}},
  ;;      "d" {:type :dir, :size 24933642, :children {}}}}

  (->> (scan-history initial-state test-data)
       :tree (#(get % root))
       (calc-size root)
       (flat-dir root))
  ;; => ({:type :dir, :size 48381165, :name "/"}
  ;;     {:type :dir, :size 94853, :name "a"}
  ;;     {:type :dir, :size 584, :name "e"}
  ;;     {:type :dir, :size 24933642, :name "d"})

  (answer1 test-data) ;; => 95437

  (def data (str/split-lines (slurp (io/resource "day7/input.txt"))))
  (answer1 data) ;; => 1348005

  (answer2 test-data) ;; => 24933642
  (answer2 data) ;; => 12785886
  )

