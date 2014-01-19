(ns examples.counters.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan put! sliding-buffer]]))

(enable-console-print!)

(def app-state
  (atom {:counters (into [] (map (fn [n] {:id n :count 0 :shared 0}) (range 10)))
         :shared ["A shared bit of UI!"]}))

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
          "-")
        (dom/label nil (:shared data))))))

(defn counters []
  (om/root
    app-state
    (fn [app owner]
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
          (dom/div nil
            (dom/h1 #js {:key "head"} "A Counting Widget!")
            (dom/div
              #js {:key "message"
                   :style 
                   (if message
                     #js {:display "block"}
                     #js {:display "none"})}
              (when message
                (str "Last clicked item was " (last message))))
            (om/build-all counter
              (map (fn [counter]
                     (update-in counter [:shared] #(om/join counter [:shared %])))
                (:counters app))
              {:key :id :init-state chans})))))
    (.getElementById js/document "app")))

(counters)
