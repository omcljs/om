(require '[cljs.repl :as repl])
(require '[cljs.repl.node :as node])

(cljs.repl/repl* (node/repl-env)
  {:verbose true
   :parallel-build true
   :compiler-stats true})
