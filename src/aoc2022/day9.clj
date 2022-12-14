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
                    (sort-by second) reverse
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

(def short-rope '(\H \T))
(def long-rope (->> (range 9)
                    (map #(char (+ (int \1) %)))
                    (cons \H)))

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

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-command)))

(def initial-short-state {\H [0 0] \T [0 0]})
(def initial-long-state
  (->> long-rope (map #(vector % p-zero)) (into {})))

(defn state-to-lps [state]
  (map (fn [[k v]] (apply lp k v)) state))

(defn move1 [pc state direction]
  (assoc state pc
         (p-plus (get state pc) direction)))

(defn move-to [tc hc state]
  (let [tp (get state tc)
        hp (get state hc)]
    (if (touching? hp tp) state
        (assoc state tc
               (p-plus tp (direction-to tp hp))))))

(defn move [hc tc state direction]
  (let [hp (get state hc)
        tp (get state tc)
        new-hp (p-plus hp direction)]
    (into state
          {\H new-hp
           \T (if (touching? new-hp tp) tp
                  (p-plus tp (direction-to tp new-hp)))})))

(defn apply-command [state rope [direction count]]
  (let [step-fn (fn [state]
                  (loop [hc (first rope)
                         st (move1 (first rope) state direction)
                         cs (rest rope)]
                    (if (empty? cs) st
                        (recur (first cs)
                               (move-to (first cs) hc st)
                               (rest cs)))))]
    (->> (iterate step-fn state)
         rest (take count))))

(defn apply-commands [state rope commands]
  (loop [state state
         commands commands
         states [state]]
    (if (empty? commands) states
        (let [next-states (apply-command state rope (first commands))]
          (recur (last next-states)
                 (rest commands)
                 (into states next-states))))))

(defn answer1 [commands]
  (->> commands
       (apply-commands initial-short-state short-rope)
       (map #(get % \T))
       set count))

(defn answer2 [commands]
  (->> commands
       (apply-commands initial-long-state long-rope)
       (map #(get % \9)) set count))

(def test-input2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(comment
  (->> [(lp \H 1 0) (lp \T 0 1)]
       (map (fn [{c :d9/char p :d9/point}] [p c]))
       (into {}))
  ;; => {[1 0] \H, [0 1] \T}
  
  (print-lps [(lp \H 1 0) (lp \T 0 1)])

  
  (->> (map parse-command ["R 4" "U 4"])
       (apply-commands (assoc initial-short-state \s p-zero)
                       short-rope)
       (map state-to-lps)
       (run! print-lps))

  (->> (map parse-command ["R 4" "U 4"])
       (apply-commands initial-short-state short-rope)
       (map #(get % \T))
       (map #(apply lp \# %))
       print-lps)
  ;; .......
  ;; .....#.
  ;; .....#.
  ;; .....#.
  ;; .####..
  ;; .......

  (answer1 (parse-input "R 4\nU 4")) ;; => 7

  (def data (parse-input (slurp (io/resource "day9/input.txt"))))
  (answer1 data) ;; => 5930

  (->> (map parse-command ["R 4" "U 4"])
       (apply-commands initial-long-state long-rope)
       (map state-to-lps)
       (run! print-lps))

  (->> (parse-input test-input2)
       (apply-commands initial-long-state long-rope)
       (map #(get % \9)) set
       (map #(apply lp \# %)) print-lps)
  ;; ........................
  ;; .#......................
  ;; .#.............###......
  ;; .#............#...#.....
  ;; ..#..........#.....#....
  ;; ...#..........#.....#...
  ;; ....#........#.......#..
  ;; .....#......#.........#.
  ;; ......#..............#..
  ;; .......#............#...
  ;; ........#..........#....
  ;; .........#........#.....
  ;; ..........########......
  ;; ........................

  (->> (parse-input test-input2)
       (apply-commands initial-long-state long-rope)
       (map #(get % \9)) set count)
  (answer2 (parse-input test-input2)) ;; => 36

  (answer2 data) ;; => 2443
  )

