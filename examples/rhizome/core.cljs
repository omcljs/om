(ns examples.rhizome.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn simple [some-data owner]
  (reify
    om/IInitState
    (init-state [_] {:number 0})
    om/IRender
    (render [_]
      (let [n (om/get-state owner :number)]
        (dom/div nil
          (dom/h2 nil (:title some-data))
          (dom/button
            #js {:onClick #(om/set-state! owner :number (inc n))}
            "Click Me!")
          (dom/p nil (str "My State: " n)))))))

(om/root
  {:some :state}
  (fn [app node]
    (om/component
      (om/build simple (om/rhizome {:title "A Rhizome!"} app))))
  (.getElementById js/document "app"))
