(ns om.devcards.core
  (:require-macros [devcards.core :refer [defcard]])
  (:require [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(defui Hello
  Object
  (render [this]
    (dom/p nil (-> this om/props :text))))

(def hello (om/create-factory Hello))

(defcard test-card
  (hello {:text "Hello, world!"}))
