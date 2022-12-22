(ns aoc2022.day16
  (:require [aoc2022.util :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]))

;; it takes you one minute to open a single valve
;; and one minute to follow any tunnel from one valve to another

(defn parse-line [line]
  (match (str/split line #"[\s=;,]+") 
         ["Valve" source "has" "flow" "rate" rate
          (:or "tunnel" "tunnels") (:or "lead" "leads") "to"
          (:or "valve" "valves") & targets]
         [source (Integer/parseInt rate) targets]))

(comment
  
  

  )
