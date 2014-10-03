(ns examples.update-props.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom
    {:widget-a {:count 0}
     :widget-b {:count 0}}))

(defn widget-a [_ owner]
  (reify
    om/IRenderProps
    (render-props [_ props _]
      (println "Widget A render!")
      (dom/div nil
        (dom/h2 nil "Widget A")
        (dom/p nil (str "Count: " (:count props)))
        (dom/button #js
          {:onClick #(om/update-props! owner props [:count] inc)}
          "+")))))

(defn widget-b [_ owner]
  (reify
    om/IRenderProps
    (render-props [_ props _]
      (println "Widget B render!")
      (dom/div nil
        (dom/h2 nil "Widget B")
        (dom/p nil (str "Count: " (:count props)))
        (dom/button #js
          {:onClick
           #(om/update-props! owner props [:count] inc)}
          "+")))))

(defn my-app [global owner]
  (reify
    om/IRender
    (render [_]
      (println "Root render!")
      (dom/div nil
        (om/build widget-a {:count 0})
        (om/build widget-b (:widget-b global))))))

(om/root my-app app-state
  {:target (.getElementById js/document "app")})
