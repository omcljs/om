(ns examples.refs.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:items [{:text "cat"}
                 {:text "dog"}
                 {:text "bird"}]}))

(def app-history (atom [@app-state]))

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
      (let [xs (items)]
        (dom/div nil
          (om/build aview {:text "View A"})
          (om/build aview {:text "View B"})
          (dom/button
            #js {:onClick
                 (fn [e] (om/transact! xs #(assoc % 1 {:text "zebra"})))}
            "Switch To Zebra!")
          (dom/button
            #js {:onClick
                 (fn [e] (om/transact! xs #(assoc % 1 {:text "dog"})))}
            "Switch to Dog!"))))))

(defn root [empty owner]
  (reify
    om/IRender
    (render [_]
      (println "Render Root")
      (dom/div nil
        (om/build main-view {})
        (dom/div #js {:id "message"} nil)
        (dom/button
          #js {:onClick
               (fn [e]
                 (when (> (count @app-history) 1)
                   (swap! app-history pop)
                   (reset! app-state (last @app-history))))}
          "Undo")))))

(om/root root app-state
  {:target (.getElementById js/document "app")})

(defn pluralize [n s]
  (if (== n 1)
    s
    (str s "s")))

(add-watch app-state :history
  (fn [_ _ _ n]
    (when-not (= (last @app-history) n)
      (swap! app-history conj n))
    (set! (.-innerHTML (.getElementById js/document "message"))
      (let [c (count @app-history)]
        (str c " Saved " (pluralize c "State"))))))
