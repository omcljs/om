(ns examples.multi.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(def app-state
  (atom
    {:widgets
     [{:type :foo
       :text "Hello!"}
      {:type :bar
       :text "Goodbye!"}]}))

(defmulti widget :type)

(defmethod widget :foo
  [props owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h2 nil "Foo Widget")
        (dom/p nil (:text props))))))

(defmethod widget :bar
  [props owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h2 nil "Bar Widget")
        (dom/p nil (:text props))))))

(defn app [props owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div nil
        (om/build-all widget (:widgets props))))))

(om/root app app-state
  {:target (.getElementById js/document "app")})
