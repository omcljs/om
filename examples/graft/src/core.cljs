(ns examples.graft.core
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
    (om/build simple (om/graft {:title "A Graft!"} app)))
  (.getElementById js/document "app0"))

;; In the following we see how we can put a real cursor in the
;; the graft value and everything still works

(def app-state (atom {:number 0}))

(defn transact-original [some-data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h2 nil (:title some-data))
        (dom/button
          #js {:onClick #(om/transact! (:app some-data) :number inc)}
          "Click Me!")
        (dom/p nil (str "My State: " (-> some-data :app :number)))))))

(om/root
  app-state
  (fn [app node]
    (om/build transact-original (om/graft {:title "Another Graft!" :app app} app)))
  (.getElementById js/document "app1"))
