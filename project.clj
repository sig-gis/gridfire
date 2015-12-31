(defproject sig-gis/gridfire "1.0.0"
  :description      "SIG's Raster-based Fire Spread and Severity Model"
  :dependencies     [[org.clojure/clojure    "1.7.0"]
                     [org.clojure/data.csv   "0.1.3"]
                     [org.clojure/java.jdbc  "0.4.2"]
                     [postgresql/postgresql  "9.3-1102.jdbc41"]
                     [net.mikera/core.matrix "0.42.0"]
                     [net.mikera/vectorz-clj "0.36.0"]
                     [lambdatronic/magellan  "0.1.0"]]
  :min-lein-version "2.5.2"
  :aot              [gridfire.cli]
  :main             gridfire.cli
  :repl-options     {:init-ns gridfire.monte-carlo}
  :global-vars      {*warn-on-reflection* true})
