(defproject om "0.1.0-SNAPSHOT"
  :description "ClojureScript interface to Facebook's react"
  :url "http://github.com/swannodette/om"
  :license {:name "Apache"
            :url "http://www.apache.org/licenses/"}

  :source-paths  ["src"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2131" :scope "provided"]
                 [com.facebook/react "0.8.0.1"]]

  :plugins [[lein-cljsbuild "1.0.0"]]

  :cljsbuild { 
    :builds [{:id "simple"
              :source-paths ["src" "test"]
              :compiler {
                :preamble ["react/react.min.js"]
                :output-to "script/tests.simple.js"
                :output-wrapper false
                :optimizations :simple}}]})
