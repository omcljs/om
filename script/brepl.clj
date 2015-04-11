(require '[cljs.repl :as repl])
(require '[cljs.repl.browser :as browser])

(cljs.repl/repl
  (browser/repl-env
    :static-dir ["resources" "resources/out"])
  :output-dir "resources/out")
