(ns examples.cursor-as-key.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom
    {:selection [0 0]
     :values
     (into {}
       (for [x (range 64)]
         [[x x] (str "Hello " x)]))}))

(defn app [app-state owner]
  (reify
    om/IRender
    (render [_]
      (let [selection (app-state :selection)
            values    (get-in app-state [:values selection])]
        (dom/div nil values)))))

(om/root app app-state
  {:target (.getElementById js/document "app")})
