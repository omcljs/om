(ns examples.counters.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan put! sliding-buffer]]))

(enable-console-print!)

(def app-state
  (atom {:counters (into [] (map (fn [n] {:id n :count 0}) (range 10)))}))

(defn counter [data owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [last-clicked]}]
      (dom/div nil
        (dom/label nil (:count data))
        (dom/button
          #js {:onClick
               (fn [e]
                 (om/transact! data :count inc)
                 (put! last-clicked (.-path data)))}
          "+")
        (dom/button
          #js {:onClick
               (fn [e]
                 (om/transact! data :count dec)
                 (put! last-clicked (.-path data)))}
          "-")))))

(defn counter-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:chans {:last-clicked (chan (sliding-buffer 1))}})
    om/IWillMount
    (will-mount [_]
      (let [last-clicked (om/get-state owner [:chans :last-clicked])]
        (go (while true
              (let [lc (<! last-clicked)]
                (om/set-state! owner :message lc))))))
    om/IRenderState
    (render-state [_ {:keys [message chans]}]
      (apply dom/div nil
        (dom/h1 #js {:key "head"} "A Counting Widget!")
        (dom/div
          #js {:key "message"
               :style 
               (if message
                 #js {:display "block"}
                 #js {:display "none"})}
          (when message
            (str "Last clicked item was " (last message))))
        (om/build-all counter (:counters app)
          {:key :id :init-state chans})))))

(om/root counter-view app-state
  {:target (.getElementById js/document "app0")
   :descriptor (om/no-local-descriptor om/no-local-state-methods)
   :tx-listen
   (fn [tx-data root-cursor]
     (println "listener 1: " tx-data))})

(om/root counter-view app-state
  {:target (.getElementById js/document "app1")
   :descriptor (om/no-local-descriptor om/no-local-state-methods)
   :tx-listen
   (fn [tx-data root-cursor]
     (println "listener 2: " tx-data))})

