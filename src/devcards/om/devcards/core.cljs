(ns om.devcards.core
  (:require-macros [devcards.core :refer [defcard]])
  (:require [om.next :as om]
            [om.dom :as dom]))

(enable-console-print!)

(defcard test-card
  "Hello")
