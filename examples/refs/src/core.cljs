(ns examples.refs.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:items [{:text "cat"}
                 {:text "dog"}
                 {:text "bird"}]}))

(defn items []
  (om/ref-cursor (:items (om/root-cursor app-state))))

(defn aview [{:keys [text]} owner]
  (reify
    om/IRender
    (render [_]
      (println "Render" text)
      (let [xs (om/observe owner (items))]
        (dom/div nil
          (dom/h2 nil text)
          (apply dom/ul nil
            (map #(dom/li nil (:text %)) xs)))))))

(defn main-view [_ owner]
  (reify
    om/IRender
    (render [_]
      (println "Render Main View")
      (dom/div nil
        (om/build aview {:text "View A"})
        (om/build aview {:text "View B"})
        (let [xs (items)]
          (dom/button
            #js {:onClick
                 (fn [e] (om/transact! xs #(assoc % 1 {:text "zebra"})))}
            "Switch!"))))))

(defn root [empty owner]
  (reify
    om/IRender
    (render [_]
      (println "Render Root")
      (dom/div nil
        (om/build main-view {})))))

(om/root root app-state
  {:target (.getElementById js/document "app")})
