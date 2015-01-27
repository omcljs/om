(ns examples.verify.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn mincase [data owner]
  (reify
    om/IWillUpdate
    (will-update [this next-props next-state]
      (.log js/console "om/IWillUpdate invoked"))
    om/IRender
    (render [_]
      (dom/div #js {:className "mincase"}
        (when (:click-to-fail data) (dom/span nil "Clicked!"))
        (dom/a
          #js {:onClick #(om/update! data :click-to-fail :done)}
          "Click me to trigger failure")))))

(om/root mincase
  (atom {})
  {:target (.getElementById js/document "app")})
