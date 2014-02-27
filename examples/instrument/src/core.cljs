(ns examples.instrument.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn sub-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil "Some subview!"))))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/div nil (:text app))
        (om/build sub-view {:foo 'bar})))))

(defn something-else [original owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:style #js {:border "1px solid #ccc"
                                :padding "5px"}}
        (dom/div nil "Instrumented!")
        (apply om/build* original)))))

(om/root
  app-view
  (atom {:text "Hello world!"})
  {:target (.getElementById js/document "app0")
   :instrument
   (fn [f cursor m]
     (om/build* something-else [f cursor m]))})
