(ns examples.multi.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom
    {:widgets
     [{:my-number 16}
      {:my-number 23}]}))

(defmulti even-odd-widget
  (fn [props _] (even? (:my-number props))))

(defmethod even-odd-widget true
  [props owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (println "Even widget mounting"))
    om/IWillUnmount
    (will-unmount [_]
      (println "Even widget unmounting"))
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h2 nil "Even Widget: " (:my-number props))
        (dom/p nil (:text props))
        (dom/button
          #js {:onClick #(om/transact! props :my-number inc)}
          "+")))))

(defmethod even-odd-widget false
  [props owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (println "Odd widget mounting"))
    om/IWillUnmount
    (will-unmount [_]
      (println "Odd widget unmounting"))
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h2 nil (str "Odd Widget: " (:my-number props)))
        (dom/p nil (:text props))
        (dom/button
          #js {:onClick #(om/transact! props :my-number inc)}
          "+")))))

(defn app [props owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div nil
        (om/build-all even-odd-widget (:widgets props))))))

(om/root app app-state
  {:target (.getElementById js/document "app")})
