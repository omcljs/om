(require '[figwheel-sidecar.repl :as r]
         '[figwheel-sidecar.repl-api :as ra])

(ra/start-figwheel!
  {:figwheel-options {}
   :build-ids ["devcards"]
   :all-builds
   [{:id "devcards"
     :figwheel {:devcards true}
     :source-paths ["src/main" "src/devcards"]
     :compiler {:main 'om.devcards.core
                :asset-path "/devcards/out"
                :output-to "resources/public/devcards/main.js"
                :output-dir "resources/public/devcards/out"
                :verbose true}}]})

(ra/cljs-repl)
