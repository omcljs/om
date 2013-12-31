(ns examples.counters.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan put! sliding-buffer]]))

(enable-console-print!)

(def app-state
  (atom {:counters (into [] (map (fn [n] {:id n :count 0 :shared 0}) (range 10)))
         :shared ["A shared bit of UI!"]}))

(defn counter [data owner chans]
  (om/component
    (dom/div nil
      (dom/label nil (:count data))
      (dom/button
        #js {:onClick
             (fn [e]
               (om/transact! data :count inc)
               (put! (:last-clicked chans) (.-path data)))}
        "+")
      (dom/button
        #js {:onClick
             (fn [e]
               (om/transact! data :count dec)
               (put! (:last-clicked chans) (.-path data)))}
        "-")
      (dom/label nil (:shared data)))))

(defn counters []
  (let [last-clicked (chan (sliding-buffer 1))
        chans {:last-clicked last-clicked}]
    (om/root
      app-state
      (fn [app owner]
        (reify
          om/IWillMount
          (will-mount [_]
            (go (while true
                  (let [lc (<! last-clicked)]
                    (om/set-state! owner :message lc)))))
          om/IRender
          (render [_]
            (dom/div nil
              (dom/h1 #js {:key "head"} "A Counting Widget!")
              (dom/div #js {:key "message"
                            :style 
                            (if (om/get-state owner :message)
                              #js {:display "block"}
                              #js {:display "none"})}
                (when-let [lc (om/get-state owner :message)]
                  (str "Last clicked item was " (last lc))))
              (om/build-all counter
                (map
                  (fn [counter]
                    (update-in counter [:shared] #(om/join counter [:shared %])))
                  (:counters app))
                {:opts chans :key :id})))))
      (.getElementById js/document "app"))))

(counters)
