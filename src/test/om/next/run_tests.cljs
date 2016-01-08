(ns om.next.run-tests
  (:require [cljs.test :refer-macros [run-tests]]
            [cljs.nodejs]
            [om.next.tests]))

(enable-console-print!)

(defn main []
  (run-tests 'om.next.tests))

(set! *main-cli-fn* main)