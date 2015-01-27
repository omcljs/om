(ns examples.shared.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:title "Shared example"}))

(defn child-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (om/get-shared owner :some-text)))))

(defn shared-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h1 nil (:title app))
        (dom/p nil
          (om/get-shared owner :some-text))
        (om/build child-view {})))))

(om/root shared-view app-state
  {:target (.getElementById js/document "app")
   :shared {:some-text "I'm shared!"}})
