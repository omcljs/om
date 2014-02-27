(ns examples.instrument.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn sub-view [item owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil (:text item)))))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/div nil (:text app))
        (apply dom/ul nil
          (om/build-all sub-view (:list app)))))))

(defn something-else [original owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:style #js {:border "1px solid #ccc"
                                :padding "5px"}}
        (dom/div nil
          (dom/span nil "Path:")
          (dom/pre #js {:style #js {:display "inline-block"}}
            (pr-str (om/path (second original)))))
        (apply om/build* original)))))

(om/root
  app-view
  (atom {:text "Instrument!"
         :list [{:text "Milk"} {:text "Cookies"} {:text "Apples"}]})
  {:target (.getElementById js/document "app")
   :instrument
   (fn [f cursor m]
     (if (= f sub-view)
       (om/build* something-else [f cursor m])
       ::om/pass))})
