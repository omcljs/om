(require '[cljs.build.api :as b])
(require '[cljs.repl :as repl])
(require '[cljs.repl.browser :as browser])

(b/build (b/inputs "src/main" "src/dev")
  {:main 'om.dev
   :asset-path "out"
   :output-to "resources/out/app.js"
   :output-dir "resources/out"
   :verbose true})

(cljs.repl/repl
  (browser/repl-env
    :static-dir ["resources" "resources/out"])
  :output-dir "resources/out"
  :asset-path "out")
