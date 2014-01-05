(ns examples.sortable.core
  (:refer-clojure :exclude [chars])
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :as async :refer
              [put! chan sliding-buffer dropping-buffer alts!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]
            [goog.events :as events]
            [goog.dom :as gdom]
            [goog.style :as gstyle])
  (:import [goog.ui IdGenerator]
           [goog.events EventType]))

(enable-console-print!)

;; =============================================================================
;; Utilities

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))

(def chars (into [] "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(defn rand-char []
  (nth chars (rand-int (count chars))))

(defn rand-word []
  (apply str (take (inc (rand-int 10)) (repeatedly rand-char))))

(defn gsize->vec [size]
  [(.-width size) (.-height size)])

(defn to? [owner next-props next-state k]
  (or (and (not (om/get-state owner k))
           (k next-state))
      (and (not (-> (om/get-props owner) :value k))
           (-> next-props :value k))))

(defn from? [owner next-props next-state k]
  (or (and (om/get-state owner k)
           (not (k next-state)))
      (and (-> (om/get-props owner) :value k)
           (not (-> next-props :value k)))))

(defn location [e]
  [(.-clientX e) (.-clientY e)])

(defn element-offset [el]
  (let [offset (gstyle/getPageOffset el)]
    [(.-x offset) (.-y offset)]))

;; =============================================================================
;; Generic Draggable

(defn dragging? [item owner]
  (or (:dragging item)
      (om/get-state owner :dragging)))

(defn drag-start [e item owner opts]
  (let [el (om/get-node owner "draggable")
        drag-start (location e)
        el-offset (element-offset el)
        drag-offset (vec (map - el-offset drag-start))]
    ;; if in a sortable need to wait for sortable to
    ;; initiate dragging
    (when-not (:delegate opts)
      (om/set-state! owner :dragging true))
    (doto owner
      (om/set-state! :location
        ((or (:constrain opts) identity) el-offset))
      (om/set-state! :drag-offset drag-offset))
    (when-let [c (:chan opts)]
      (put! c
        {:event :drag-start
         :id (:id item)
         :location (vec (map + drag-start drag-offset))}))))

(defn drag-stop [e item owner opts]
  (when (dragging? item owner)
    (when (om/get-state owner :dragging)
      (om/set-state! owner :dragging false))
    (doto owner
      (om/set-state! :location nil)
      (om/set-state! :drag-offset nil))
    (when-let [c (:chan opts)]
      (put! c {:event :drag-stop :id (:id item)}))))

(defn drag [e item owner opts]
  (let [state (om/get-state owner)]
    (when (dragging? item owner)
      (let [loc ((or (:constrain opts) identity)
                  (vec (map + (location e) (:drag-offset state))))]
        (om/set-state! owner :location loc)
        (when-let [c (:chan opts)]
          (put! c {:event :drag :location loc}))))))

(defn draggable [item owner opts]
  (reify
    om/IDidMount
    (did-mount [_ _]
      ;; capture the cell dimensions when it becomes available
      (let [dims (om/get-state owner :dimensions)]
        (when-not dims
          (let [dims (-> owner
                       (om/get-node "draggable")
                       gstyle/getSize
                       gsize->vec)]
            (om/set-state! owner :dimensions dims)
            ;; let cell dimension listeners know
            (when-let [dims-chan (:dims-chan opts)]
              (put! dims-chan dims))))))
    om/IWillUpdate
    (will-update [_ next-props next-state]
      ;; begin dragging, need to track events on window
      (when (or (to? owner next-props next-state :dragging))
        (let [mouse-up   (om/pure-bind drag-stop (:value next-props) owner opts)
              mouse-move (om/pure-bind drag (:value next-props) owner opts)]
          (om/set-state! owner :window-listeners
            [mouse-up mouse-move])
          (doto js/window
            (events/listen EventType.MOUSEUP mouse-up)
            (events/listen EventType.MOUSEMOVE mouse-move))))
      ;; end dragging, cleanup window event listeners
      (when (from? owner next-props next-state :dragging)
        (let [[mouse-up mouse-move]
              (om/get-state owner :window-listeners)]
          (doto js/window
            (events/unlisten EventType.MOUSEUP mouse-up)
            (events/unlisten EventType.MOUSEMOVE mouse-move)))))
    om/IRender
    (render [_]
      (let [state (om/get-state owner)
            style (cond
                    (dragging? item owner)
                    (let [[x y] (:location state)
                          [w h] (:dimensions state)]
                      #js {:position "absolute"
                           :top y :left x :z-index 1
                           :width w :height h})
                    :else
                    #js {:position "static" :z-index 0})]
        (dom/li
          #js {:className (when (dragging? item owner) "dragging")
               :style style
               :ref "draggable"
               :onMouseDown (om/pure-bind drag-start item owner opts)
               :onMouseUp   (om/pure-bind drag-stop item owner opts)
               :onMouseMove (om/pure-bind drag item owner opts)}
          (om/build (:view opts) item {:opts opts}))))))

;; =============================================================================
;; Generic Sortable

(defn from-loc [v1 v2]
  (vec (map - v2 v1)))

(defn sortable-spacer [height]
  (dom/li
    #js {:key "spacer-cell"
         :style #js {:height height}}))

(defn index-of [x v]
  (loop [i 0 v (seq v)]
    (if v
      (if (= x (first v))
        i
        (recur (inc i) (next v)))
      -1)))

(defn insert-at [x idx v]
  (if (== idx (count v))
    (conj v x)
    (vec (concat (take idx v) [x] (drop idx v)))))

(defn start-sort [owner e]
  (let [state (om/get-state owner)
        sort  (:sort state)
        idx   (index-of (:id e) sort)]
    (doto owner
      (om/set-state! :sorting (:id e))
      (om/set-state! :real-sort sort)
      (om/set-state! :drop-index idx)
      (om/set-state! :sort (insert-at ::spacer idx sort)))))

(defn handle-drop [owner e]
  (let [{:keys [sort drop-index]} (om/get-state owner)
        idx (index-of ::spacer sort)
        sort (->> sort
               (remove #{(:id e)})
               (replace {::spacer (:id e)})
               vec)]
    (doto owner
      (om/set-state! :sorting nil)
      (om/set-state! :drop-index nil)
      (om/set-state! :real-sort nil)
      (om/set-state! :sort sort))))

(defn update-drop [owner [x y :as loc]]
  (let [state  (om/get-state owner)
        [_ y]  (from-loc (:location state) loc)
        [_ ch] (:cell-dimensions state)
        drop-index (js/Math.round (inc (/ y ch)))]
    (when (not= (:drop-index state) drop-index)
      (om/set-state! owner :sort
        (vec (insert-at ::spacer drop-index (:real-sort state)))))))

(defn bound [n lb ub]
  (cond
    (< n lb) lb
    (> n ub) ub
    :else n))

(defn handle-drag-event [owner e]
  (case (:event e)
    :drag-start (start-sort owner e) 
    :drag-stop  (handle-drop owner e)
    :drag       (update-drop owner (:location e))
    nil))

(defn sortable [{:keys [items sort]} owner opts]
  (reify
    om/IInitState
    (init-state [_] {:sort (om/value sort)})
    om/IWillMount
    (will-mount [_]
      (let [drag-chan (chan)
            dims-chan (chan (dropping-buffer 1))]
        (om/set-state! owner :chans
          {:drag-chan drag-chan
           :dims-chan dims-chan})
        (go
          (while true
            (alt!
              drag-chan ([e c] (handle-drag-event owner e))
              dims-chan ([e c] (om/set-state! owner :cell-dimensions e)))))))
    ;; doesn't account for change in number of items in sortable
    ;; nor changes in sort - exercise for the reader 
    om/IWillUpdate
    (will-update [_ next-props next-state]
      ;; calculate constraints from cell-dimensions when we receive them
      (when (to? owner next-props next-state :cell-dimensions)
        (let [node   (om/get-node owner "sortable") 
              [w h]  (gsize->vec (gstyle/getSize node))
              [x y]  (element-offset node)
              [_ ch] (:cell-dimensions next-state)]
          (om/set-state! owner :constrain
            (fn [[_ cy]] [(inc x) (bound cy y (- (+ y h) ch))])))))
    om/IDidMount
    (did-mount [_ _]
      (when-not (om/get-state owner :location)
        (om/set-state! owner :location
          (element-offset (om/get-node owner "sortable")))))
    om/IRender
    (render [_]
      (let [state (om/get-state owner)]
        (dom/ul #js {:className "sortable" :ref "sortable"}
          (into-array
            (map
              (fn [id]
                (if-not (= id ::spacer)
                  (om/build draggable (items id)
                    {:key :id
                     :opts (let [{:keys [constrain chans]} state]
                             (assoc opts 
                               :constrain constrain
                               :chan      (:drag-chan chans)
                               :dims-chan (:dims-chan chans)
                               :delegate  true))
                     :fn (fn [x]
                           (if (= (:id x) (:sorting state))
                             (assoc x :dragging true)
                             x))})
                  (sortable-spacer (second (:cell-dimensions state)))))
              (:sort state))))))))

;; =============================================================================
;; Example

(def app-state
  (let [items (->> (take 10 (repeatedly guid))
                (map (fn [id] [id {:id id :text (rand-word)}]))
                (into {}))]
    (atom {:items items
           :sort (into [] (keys items))})))

(defn item [the-item owner opts]
  (om/component (dom/span nil (str "Item " (:text the-item)))))

(om/root app-state
  (fn [app owner]
    (om/component
      (dom/div nil
        (dom/h2 nil "Sortable example")
        (om/build sortable app {:opts {:view item}}))))
  (.getElementById js/document "app"))
