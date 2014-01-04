(ns examples.sortable.core
  (:refer-clojure :exclude [chars])
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :as async :refer [chan sliding-buffer]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string])
  (:import [goog.ui IdGenerator]))

(enable-console-print!)

;; =============================================================================
;; Utilities

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))

;; =============================================================================
;; Generic Sortable

(defn sortable-item [item owner opts]
  (om/component
    (dom/li
      #js {:onMouseDown (fn [e])
           :onMouseUp   (fn [e])
           :onMouseMove (fn [e])}
      (om/build (:view opts) item {:opts opts}))))

(defn sortable [items owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/ul nil (om/build-all sortable-item items {:opts opts})))))

;; =============================================================================
;; Example

(def app-state
  (atom {:items
         (->> (take 10 (range))
           (map (fn [idx] {:id (guid) :index idx :type :item}))
           (into []))}))

(defn item [the-item owner opts]
  (om/component (dom/span nil (str "Item " (:id the-item)))))

(om/root app-state
  (fn [app owner]
    (reify
      om/IWillMount
      (will-mount [_]
        (let [{:keys [start drag stop] :as chans}
              (zipmap [:start :drag :stop]
                      (repeatedly #(chan (sliding-buffer 1))))]
          (om/set-state! owner :chans chans)
          (go (while true
                (alt!
                  start ([v c] (println "start"))
                  drag  ([v c] (println "drag"))
                  stop  ([v c] (println "stop")))))))
      om/IRender
      (render [_]
        (dom/div nil
          (dom/h2 nil "Sortable example")
          (om/build sortable (:items app)
            {:opts {:view item :chans (om/get-state owner :chans)}})))))
  (.getElementById js/document "app"))
