(require '[figwheel-sidecar.repl :as r]
         '[figwheel-sidecar.repl-api :as ra])

(ra/start-figwheel!
  {:figwheel-options {}
   :build-ids ["devcards"]
   :all-builds
   [{:id "devcards"
     :source-paths ["src" "dev"]
     :compiler }]})

(ra/cljs-repl)
