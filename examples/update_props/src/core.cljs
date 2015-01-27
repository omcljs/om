(ns examples.update-props.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:widget {:count 0}}))

(defn widget [_ owner]
  (reify
    om/IRenderProps
    (render-props [_ props _]
      (println "Widget render!")
      (dom/div nil
        (dom/h2 nil "A Widget")
        (dom/p nil (str "Count: " (:count props)))
        (dom/button #js
          {:onClick #(om/update-props! owner props [:count] inc)}
          "+")))))

(defn my-app [global owner]
  (reify
    om/IRender
    (render [_]
      (println "Root render!")
      (dom/div nil
        (om/build widget (:widget global))))))

(om/root my-app app-state
  {:target (.getElementById js/document "app")})
