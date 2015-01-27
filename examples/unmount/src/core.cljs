(ns examples.unmount.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn widget-a [data owner]
  (reify
    om/IWillUnmount
    (will-unmount [_]
      (println "umounting Widget A"))
    om/IRender
    (render [_]
      (dom/div nil "Widget A"))))

(defn widget-b [data owner]
  (reify
    om/IWillUnmount
    (will-unmount [_]
      (println "umounting Widget B"))
    om/IRender
    (render [_]
      (dom/div nil "Widget B"))))

(defn app [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (if (= (:widget data) :a)
          (om/build widget-a {})
          (om/build widget-b {}))
        (dom/button
          #js {:onClick (fn [e] (om/transact! data :widget {:a :b :b :a}))}
          "Switch!")))))

(om/root app {:widget :a}
  {:target (.getElementById js/document "app")})
