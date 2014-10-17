(ns examples.hello.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn widget [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:count 0})
    om/IRenderState
    (render-state [_ {:keys [count]}]
      (println "Render!")
      (dom/div nil
        (dom/h2 nil "Hello world!")
        (dom/p nil (str "Count: " count))
        (dom/button #js {:onClick #(om/update-state! owner :count inc)}
          "Bump!")
        (dom/button #js {:onClick #(om/update-state! owner :count identity)}
          "Do Nothing")))))

(om/root widget {}
  {:target (.getElementById js/document "app")})
