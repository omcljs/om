(ns examples.input.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(om/root
  (fn [app owner]
    (reify 
      om/IInitState
      (init-state [_]
        {:value "" :count 0})
      om/IRenderState
      (render-state [_ {:keys [value]}]
        (dom/div nil
          (dom/label nil "Only numeric : ")
          (dom/input #js
            {:value value
             :onChange
             #(let [new-value (-> % .-target .-value)]
                (if (js/isNaN new-value)
                  (om/set-state! owner :value value)
                  (om/set-state! owner :value new-value)))})))))
  {}
  {:target (. js/document (getElementById "app"))})

