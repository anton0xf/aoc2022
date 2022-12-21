(ns aoc2022.util)

(defn get-bounds [gap ps]
  (let [bounds-fn (fn [map-fn reduce-fn result-fn]
                    (->> ps (map map-fn) (reduce reduce-fn) (result-fn gap)))]
    [[(bounds-fn first min #(- %2 %1))
      (bounds-fn second min #(- %2 %1))]
     [(bounds-fn first max +)
      (bounds-fn second max +)]]))
