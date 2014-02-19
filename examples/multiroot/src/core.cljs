(ns examples.multiroot.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:text "Hello!"
         :modal1 {:text "Modal 1"}
         :modal2 {:text "Modal 2"}}))

(defn display [data owner]
  (om/component
    (dom/div nil (:text data))))

(om/root display app-state {:target (.getElementById js/document "app1")})
(om/root display app-state {:target (.getElementById js/document "app2")})

(swap! app-state assoc :text "Goodbye!")

(defn modal [data owner]
  (om/component
    (dom/div nil (:text data))))

(om/root modal app-state
  {:target (.getElementById js/document "modal1")
   :path [:modal1]})

(om/root modal app-state
  {:target (.getElementById js/document "modal2")
   :path [:modal2]})
