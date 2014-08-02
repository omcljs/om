(ns examples.multi.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(def app-state
  (atom
    {:widgets
     [{:my-number 16
       :text "I'm divisible by 2!"}
      {:my-number 23
       :text "I'm not divisible by 2!"}]}))

(defmulti even-odd-widget (comp even? :my-number))

(defmethod even-odd-widget true
  [props owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h2 nil "Even Widget")
        (dom/p nil (:text props))))))

(defmethod even-odd-widget false
  [props owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h2 nil "Odd Widget")
        (dom/p nil (:text props))))))

(defn app [props owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div nil
        (om/build-all even-odd-widget (:widgets props))))))

(om/root app app-state
  {:target (.getElementById js/document "app")})
