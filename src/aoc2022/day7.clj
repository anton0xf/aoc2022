(ns aoc2022.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]))

(def root "/")
(def initial-state {:path []
                    :tree [{:type :dir :name root}]})
(comment
  {:type :dir
   :children [{:type :dir :name "a"}
              {:type :file :name "b.txt" :size 12}]})

(def command-prefix "$ ")

(defn command? [s]
  (str/starts-with? s command-prefix))

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
    (if (= "dir" type-or-size)
      {:type :dir :name name}
      {:type :file :name name
       :size (Integer/parseInt type-or-size)})))

(defn ls-dirs [path dirs content]
  (letfn [(ls-dir [dir]
            (if (= (first path) (:name dir))
              (if (empty? (rest path))
                (assoc dir :children content)
                (update dir :children #(ls-dirs (rest path) % content)))
              dir))]
    (if (empty? path) dirs
        (map ls-dir dirs))))

(defn ls [state content]
  (update state :tree
          #(ls-dirs (:path state) %
                    (map parse-content-line content))))

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

(defn traverse-dir [dir file-fn dir-fn]
  (->> (:children dir)
       (map (fn [val]
              (match (:type val)
                     :dir (traverse-dir val file-fn dir-fn)
                     :file (file-fn val))))
       (dir-fn dir)))

(defn calc-size [dir]
  (traverse-dir
   dir
   identity
   (fn [dir acc]
     (assoc
      (if-let [children (seq (filter #(= :dir (:type %)) acc))]
        (assoc dir :children children)
        (dissoc dir :children))
      :size (reduce + (map :size acc))))))

(defn flat-dir [dir]
  (traverse-dir
   dir identity
   (fn [dir acc] (cons (dissoc dir :children)
                       (reduce concat acc)))))

(defn answer1 [lines]
  (->> (scan-history initial-state lines)
       :tree first calc-size flat-dir
       (map :size)
       (filter #(>= 100000 %))
       (reduce +)))

(defn answer2 [lines]
  (let [root-with-size
        (->> (scan-history initial-state lines)
             :tree first calc-size)
        used (:size root-with-size)
        max-used (- 70000000 30000000)
        remove-at-least (- used max-used)]
    (->> root-with-size
         flat-dir (map :size)
         (filter #(<= remove-at-least %))
         (reduce min))))

(comment
  (def test-data (str/split-lines (slurp (io/resource "day7/test-input.txt"))))
  (scan-history initial-state test-data)
  ;; => {:path ["/" "d"],
  ;;     :tree
  ;;     ({:type :dir,
  ;;       :name "/",
  ;;       :children
  ;;       ({:type :dir,
  ;;         :name "a",
  ;;         :children
  ;;         ({:type :dir,
  ;;           :name "e",
  ;;           :children ({:type :file, :name "i", :size 584})}
  ;;          {:type :file, :name "f", :size 29116}
  ;;          {:type :file, :name "g", :size 2557}
  ;;          {:type :file, :name "h.lst", :size 62596})}
  ;;        {:type :file, :name "b.txt", :size 14848514}
  ;;        {:type :file, :name "c.dat", :size 8504156}
  ;;        {:type :dir,
  ;;         :name "d",
  ;;         :children
  ;;         ({:type :file, :name "j", :size 4060174}
  ;;          {:type :file, :name "d.log", :size 8033020}
  ;;          {:type :file, :name "d.ext", :size 5626152}
  ;;          {:type :file, :name "k", :size 7214296})})})}

  (-> (scan-history initial-state test-data)
      :tree first calc-size)
  ;; => {:type :dir,
  ;;     :name "/",
  ;;     :children
  ;;     ({:type :dir,
  ;;       :name "a",
  ;;       :children ({:type :dir, :name "e", :size 584}),
  ;;       :size 94853}
  ;;      {:type :dir, :name "d", :size 24933642}),
  ;;     :size 48381165}

  (->> (scan-history initial-state test-data)
       :tree first calc-size flat-dir)
  ;; => ({:type :dir, :name "/", :size 48381165}
  ;;     {:type :dir, :name "a", :size 94853}
  ;;     {:type :dir, :name "e", :size 584}
  ;;     {:type :dir, :name "d", :size 24933642})

  (answer1 test-data) ;; => 95437

  (def data (str/split-lines (slurp (io/resource "day7/input.txt"))))
  (answer1 data) ;; => 1348005

  (answer2 test-data) ;; => 24933642
  (answer2 data) ;; => 12785886
  )

