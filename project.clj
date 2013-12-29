(defproject om "0.1.0-SNAPSHOT"
  :description "ClojureScript interface to Facebook's react"
  :url "http://github.com/swannodette/om"
  :license {:name "Eclipse"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jvm-opts ^:replace ["-Xmx512m" "-server"]
  
  :source-paths  ["src"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2127" :scope "provided"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha" :scope "provided"]
                 [com.facebook/react "0.8.0.1"]]

  :plugins [[lein-cljsbuild "1.0.1"]]

  :cljsbuild { 
    :builds [{:id "test"
              :source-paths ["src" "test"]
              :compiler {
                :preamble ["react/react.min.js"]
                :output-to "script/tests.simple.js"
                :output-wrapper false
                :optimizations :simple}}
             ;; examples
             {:id "hello"
              :source-paths ["src" "examples/hello"]
              :compiler {
                :output-to "examples/hello/main.js"
                :output-dir "examples/hello/out"
                :source-map true
                :optimizations :none}}
             {:id "mouse"
              :source-paths ["src" "examples/mouse"]
              :compiler {
                :output-to "examples/mouse/main.js"
                :output-dir "examples/mouse/out"
                :source-map true
                :optimizations :none}}
             {:id "counters"
              :source-paths ["src" "examples/counters"]
              :compiler {
                :output-to "examples/counters/main.js"
                :output-dir "examples/counters/out"
                :source-map true
                :optimizations :none}}]})
