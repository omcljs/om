(ns examples.animation.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:count 0}))

(defn animation-view [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (js/setInterval
        (fn [] (om/transact! app :count inc))
        16))
    om/IRender
    (render [_]
      (dom/div nil (:count app)))))

(om/root animation-view app-state
  {:target (.getElementById js/document "app")})
