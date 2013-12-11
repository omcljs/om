(defproject reactjs "0.5.1.1"
  :description "A Javascript Library for building user interfaces"
  :url "http://facebook.github.io/react/"
  :license {:name "Apache"
            :url "http://www.apache.org/licenses/"}

  :source-paths  ["src"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2120"]]

  :plugins [[lein-cljsbuild "1.0.0"]]

  :cljsbuild { 
    :builds [{:id "dev"
              :source-paths ["src"]
              :compiler {
                :output-to "dev.js"
                :output-dir "out"
                :optimizations :none}}]})
