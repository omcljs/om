(require '[cljs.build.api :as b])

(b/watch (b/inputs "src/main" "src/test/om/next")
  {:main 'om.next.tests
   :output-to "target/test/main.js"
   :output-dir "target/test/out"
   :parallel-build true
   :compiler-stats true
   :verbose true})

(System/exit 0)