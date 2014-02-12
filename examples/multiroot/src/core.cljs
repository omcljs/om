(ns examples.multiroot.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:text "Hello!"}))

(defn display [data owner]
  (om/component
    (dom/div nil (:text data))))

(om/root display app-state {:target (.getElementById js/document "app1")})
(om/root display app-state {:target (.getElementById js/document "app2")})

(swap! app-state assoc :text "Goodbye!")
