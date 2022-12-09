(ns aoc2022.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [clojure.spec.alpha :as s]))

(s/def :d9/char char?)
(s/def :d9/point (s/coll-of int? :count 2))
;; labeled point
(s/def :d9/lp (s/keys :req [:d9/char :d9/point]))

(defn lp [c i j]
  {:pre [(s/valid? char? c)
         (s/valid? int? i)
         (s/valid? int? j)]
   :post [(s/valid? :d9/lp %)]}
  {:d9/char c :d9/point [i j]})

(defn get-bounds [lps gap]
  {:pre [(s/valid? (s/coll-of :d9/lp) lps)]
   :post [(s/valid? (s/coll-of (s/coll-of int? :count 2) :count 2) %)]}
  (let [ps (map :d9/point lps)
        bounds-fn (fn [map-fn reduce-fn result-fn]
                    (->> ps (map map-fn) (reduce reduce-fn) result-fn))]
    [[(bounds-fn first min #(- % gap))
      (bounds-fn second min #(- % gap))]
     [(bounds-fn first max #(+ % gap))
      (bounds-fn second max #(+ % gap))]]))

(defn format-lps [lps]
  {:pre [(s/valid? (s/coll-of :d9/lp) lps)]
   :post [(s/valid? string? %)]}
  (let [[[i-min j-min] [i-max j-max]] (get-bounds lps 1)
        ps-map (->> lps
                    (map (fn [{c :d9/char p :d9/point}] [p c]))
                    (into {}))]
    (str/join
     "\n" (for [i (range i-min (inc i-max))]
            (str/join
             (for [j (range j-min (inc j-max))]
               (if-let [c (get ps-map [i j])] c \.)))))))

(def p-zero [0 0])

(defn p-plus [p1 p2]
  {:pre [(s/valid? :d9/point p1)
         (s/valid? :d9/point p2)]
   :post [(s/valid? :d9/point %)]}
  (map + p1 p2))

(defn p-mult [p k]
  (map #(* k %) p))

(defn p-minus [p1 p2]
  {:pre [(s/valid? :d9/point p1)
         (s/valid? :d9/point p2)]
   :post [(s/valid? :d9/point %)]}
  (map - p1 p2))

(defn touching? [hp tp]
  (->> (p-minus hp tp)
       (map abs)
       (apply max)
       (>= 1)))

(defn print-lps [lps]
  (newline) (newline)
  (println (format-lps lps)))

(defn scalar-direction [d]
  (if (= 0 d) 0
      (/ d (abs d))))

(defn direction-to [tp hp]
  (let [r (p-minus hp tp)]
    (if (= p-zero r) p-zero
        (map scalar-direction r))))

(def directions {"R" [0 1], "L" [0 -1], "U" [-1 0], "D" [1 0]})

(defn parse-command [s]
  (let [[command count-str] (str/split s #" ")]
    [(directions command)
     (Integer/parseInt count-str)]))

(def initial-state {\H [0 0] \T [0 0]})

(defn state-to-lps [state]
  (map (fn [[k v]] (apply lp k v)) state))

(defn apply-command [state [direction count]]
  (let [new-h (-> (get state \H) (p-plus (p-mult )) )]))
(comment
  (->> [(lp \H 1 0) (lp \T 0 1)]
       (map (fn [{c :d9/char p :d9/point}] [p c]))
       (into {}))
  ;; => {[1 0] \H, [0 1] \T}
  
  (print-lps [(lp \H 1 0) (lp \T 0 1)])

  
  (apply-command initial-state [[0 1] 4])

  )

