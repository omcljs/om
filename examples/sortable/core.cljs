(ns examples.sortable.core
  (:refer-clojure :exclude [chars])
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :as async :refer [chan sliding-buffer]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]
            [goog.events :as events])
  (:import [goog.ui IdGenerator]
           [goog.events EventType]))

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
      #js {:onMouseDown (fn [e] (println "mouse down"))
           :onMouseUp   (fn [e] (println "mouse up"))
           :onMouseMove (fn [e] (println "mouse move"))}
      (om/build (:view opts) item {:opts opts}))))

(defn sortable [items owner opts]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [mouse-down (fn [e] (println "window mouse down"))
            mouse-up   (fn [e] (println "window mouse up"))
            mouse-move (fn [e] (println "window mouse move"))]
        (om/set-state! owner :window-listener
          [mouse-down mouse-up mouse-move])
        (doto js/window
          (events/listen EventType.MOUSEDOWN mouse-down)
          (events/listen EventType.MOUSEUP mouse-up)
          (events/listen EventType.MOUSEMOVE mouse-move))))
    om/IWillUnmount
    (will-unmount [_]
      (let [[mouse-down mouse-up mouse-move]
            (om/get-state! owner :window-listeners)]
        (doto js/window
          (events/unlisten EventType.MOUSEDOWN mouse-down)
          (events/unlisten EventType.MOUSEUP mouse-up)
          (events/unlisten EventType.MOUSEMOVE mouse-move))))
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
