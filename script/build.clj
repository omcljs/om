(require '[cljs.build.api :as b])

(b/build (b/inputs "src/main" "src/devcards")
  {:main 'om.devcards.core
   :asset-path "/devcards/out"
   :output-to "resources/public/devcards/main.js"
   :output-dir "resources/public/devcards/out"
   :parallel-build true
   :compiler-stats true
   :verbose true})

(System/exit 0)
