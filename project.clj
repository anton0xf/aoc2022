(defproject aoc2022 "0.1.0-SNAPSHOT"
  :description "anton0xf's solutions for Advent of Code 2022"
  :url "https://github.com/anton0xf/aoc2022"
  :license {:name "MIT license"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]]
  :main ^:skip-aot aoc2022.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
