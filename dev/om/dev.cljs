(ns om.dev
  (:require [clojure.browser.repl :as repl]
            [om.core :as om]
            [om.dom :as dom]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))
