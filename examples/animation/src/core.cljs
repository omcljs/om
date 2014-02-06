(ns examples.animation.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:count 0}))

(om/root
  app-state
  (fn [app owner]
    (reify
      om/IWillMount
      (will-mount [_]
        (js/setInterval
          (fn [] (om/transact! app :count inc))
          16))
      om/IRender
      (render [_]
        (dom/div nil (:count app)))))
  (.getElementById js/document "app"))
