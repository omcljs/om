(ns examples.state-bug.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {}))

(defn child-view [_ _ {:keys [inform!]}]
  (reify
    om/IRender
    (render [_]
      (inform!)
      (dom/div nil "Child"))))

(defn parent-view [_ owner]
  (reify
    om/IRenderState
    (render-state
      [_ {:keys [foo] :as state}]
      (dom/div nil
        (str "Foo is: " (pr-str foo))
        (dom/div nil
          (om/build child-view
            nil
            {:opts {:inform! (fn [] (om/set-state! owner :foo "bar"))}}))))))

(defn application [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:cfg-init false})
    om/IWillMount
    (will-mount [_]
      (js/setTimeout #(om/set-state! owner :cfg-init true) 500))
    om/IRenderState
    (render-state [_ {:keys [cfg-init]}]
      ;; this works fine
      #_(om/build parent-view {})
      ;; start problem code
      ;; comment out this code and uncomment the above to see it work
      (if cfg-init
        (om/build parent-view {})
        (dom/div nil))
      ;; end problem code
      )))

(defn start [dom-id]
  (om/root application app-state
    {:target (.getElementById js/document dom-id)}))

(start "app")
