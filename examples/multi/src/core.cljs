(ns examples.multi.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(def app-state
  (atom
    {:widgets
     [{:type :foo
       :text "Foo Widget!"}
      {:type :bar
       :text "Bar Widget!"}]}))

(defmulti widget :type)

(defmethod widget :foo
  [props owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil (:text props)))))

(defmethod widget :bar
  [props owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil (:text props)))))

(defn app [props owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div nil
        (om/build-all widget (:widgets props))))))

(om/root app app-state
  {:target (.getElementById js/document "app")})
