(defproject om "0.1.0-SNAPSHOT"
  :description "ClojureScript interface to Facebook's react"
  :url "http://github.com/swannodette/om"
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
