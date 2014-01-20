(ns examples.shared.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:title "Shared example"}))

(om/root
  app-state
  {:some-text "I'm shared!"}
  (fn [app owner]
    (reify
      om/IRender
      (render [_]
        (dom/div nil
          (dom/h1 nil (:title app))
          (dom/p nil
            (om/get-shared owner :some-text))))))
  (.getElementById js/document "app"))
