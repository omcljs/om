(ns examples.sortable.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :as async :refer [put! chan dropping-buffer]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as events]
            [goog.style :as gstyle])
  (:import [goog.ui IdGenerator]
           [goog.events EventType]))

(enable-console-print!)

;; =============================================================================
;; Utilities

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))

(defn gsize->vec [size]
  [(.-width size) (.-height size)])

(defn to? [owner next-props next-state k]
  (or (and (not (om/get-render-state owner k))
           (k next-state))
      (and (not (k (om/get-props owner)))
           (k next-props))))

(defn from? [owner next-props next-state k]
  (or (and (om/get-render-state owner k)
           (not (k next-state)))
      (and (k (om/get-props owner))
           (not (k next-props)))))

(defn location [e]
  [(.-clientX e) (.-clientY e)])

(defn element-offset [el]
  (let [offset (gstyle/getPageOffset el)]
    [(.-x offset) (.-y offset)]))

;; =============================================================================
;; Generic Draggable

(defn dragging? [owner]
  (om/get-state owner :dragging))

(defn drag-start [e item owner]
  (when-not (dragging? owner)
    (let [el          (om/get-node owner "draggable")
          state       (om/get-state owner)
          drag-start  (location e)
          el-offset   (element-offset el)
          drag-offset (vec (map - el-offset drag-start))]
      ;; if in a sortable need to wait for sortable to
      ;; initiate dragging
      (when-not (:delegate state)
        (om/set-state! owner :dragging true))
      (doto owner
        (om/set-state! :location
          ((or (:constrain state) identity) el-offset))
        (om/set-state! :drag-offset drag-offset))
      (when-let [c (:chan state)]
        (put! c
          {:event :drag-start
           :id (:id item)
           :location (vec (map + drag-start drag-offset))})))))

(defn drag-stop [e item owner]
  (when (dragging? owner)
    (let [state (om/get-state owner)]
      (when (:dragging state)
        (om/set-state! owner :dragging false))
      ;; rendering order issues otherwise
      (when-not (:delegate state)
        (doto owner
          (om/set-state! :location nil)
          (om/set-state! :drag-offset nil)))
      (when-let [c (:chan state)]
        (put! c {:event :drag-stop :id (:id item)})))))

(defn drag [e item owner]
  (let [state (om/get-state owner)]
    (when (dragging? owner)
      (let [loc ((or (:constrain state) identity)
                  (vec (map + (location e) (:drag-offset state))))]
        (om/set-state! owner :location loc)
        (when-let [c (:chan state)]
          (put! c {:event :drag :location loc :id (:id item)}))))))

(defn draggable [item owner]
  (reify
    om/IDidMount
    (did-mount [_]
      ;; capture the cell dimensions when it becomes available
      (let [dims (-> (om/get-node owner "draggable")
                     gstyle/getSize gsize->vec)]
        (om/set-state! owner :dimensions dims)
        ;; let cell dimension listeners know
        (when-let [dims-chan (:dims-chan (om/get-state owner))]
          (put! dims-chan dims))))
    om/IWillUpdate
    (will-update [_ next-props next-state]
      ;; begin dragging, need to track events on window
      (when (or (to? owner next-props next-state :dragging))
        (let [mouse-up   #(drag-stop % @next-props owner)
              mouse-move #(drag % @next-props owner)]
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
    om/IRenderState
    (render-state [_ state]
      (let [style (cond
                    (dragging? owner)
                    (let [[x y] (:location state)
                          [w h] (:dimensions state)]
                      #js {:position "absolute"
                           :top y :left x :z-index 1
                           :width w :height h})
                    :else
                    #js {:position "static" :z-index 0})]
        (dom/li
          #js {:className (when (dragging? owner) "dragging")
               :style style
               :ref "draggable"
               :onMouseDown #(drag-start % @item owner)
               :onMouseUp   #(drag-stop % @item owner)
               :onMouseMove #(drag % @item owner)}
          (om/build (:view state) item))))))

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

(defn insert-at [x idx ignore v]
  (let [len (count v)]
    (loop [i 0 v v ret []]
      (if (>= i len)
        (conj ret x)
        (let [y (first v)]
          (if (= y ignore)
            (recur i (next v) (conj ret y))
            (if (== i idx)
              (into (conj ret x) v)
              (recur (inc i) (next v) (conj ret y)))))))))

(defn sorting? [owner]
  (om/get-state owner :sorting))

(defn start-sort [owner e]
  (let [state (om/get-state owner)
        sort  (:sort state)
        idx   (index-of (:id e) sort)]
    (doto owner
      (om/set-state! :sorting (:id e))
      (om/set-state! :real-sort sort)
      (om/set-state! :drop-index idx)
      (om/set-state! :sort (insert-at ::spacer idx (:id e) sort)))))

(defn handle-drop [owner e]
  (when (sorting? owner)
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
        (om/set-state! :sort sort)))))

(defn update-drop [owner e]
  (when (sorting? owner)
    (let [loc    (:location e)
          state  (om/get-state owner)
          [_ y]  (from-loc (:location state) loc)
          [_ ch] (:cell-dimensions state)
          drop-index (js/Math.round (/ y ch))]
      (when (not= (:drop-index state) drop-index)
        (doto owner
          (om/set-state! :drop-index drop-index)
          (om/set-state! :sort
            (insert-at ::spacer drop-index (:id e) (:real-sort state))))))))

(defn bound [n lb ub]
  (cond
    (< n lb) lb
    (> n ub) ub
    :else n))

(defn handle-drag-event [owner e]
  (case (:event e)
    :drag-start (start-sort owner e) 
    :drag-stop  (handle-drop owner e)
    :drag       (update-drop owner e)
    nil))

(defn sortable [{:keys [items sort]} owner]
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
    (did-mount [_]
      (when-not (om/get-state owner :location)
        (om/set-state! owner :location
          (element-offset (om/get-node owner "sortable")))))
    om/IRenderState
    (render-state [_ state]
      (apply dom/ul #js {:className "sortable" :ref "sortable"}
        (map
          (fn [id]
            (if-not (= id ::spacer)
              (om/build draggable (items id)
                (let [{:keys [constrain chans view]} state]
                  {:key :id
                   :init-state {:view      view
                                :chan      (:drag-chan chans)
                                :dims-chan (:dims-chan chans)
                                :delegate  true}
                   :state {:constrain constrain
                           :dragging  (= id (:sorting state))}}))
              (sortable-spacer (second (:cell-dimensions state)))))
          (:sort state))))))

;; =============================================================================
;; Example

(def app-state
  (let [items (->> (take 10 (map vector (repeatedly guid) (range)))
                (map (fn [[id n]] [id {:id id :title n}]))
                (into {}))]
    (atom {:items items
           :sort (into [] (keys items))})))

(defn item [the-item owner]
  (om/component (dom/span nil (str "Item " (:title the-item)))))

(defn sortable-view [app owner]
  (om/component
    (dom/div nil
      (dom/h2 nil "Sortable example")
      (om/build sortable app {:init-state {:view item}}))))

(om/root sortable-view app-state
  {:target (.getElementById js/document "app")})
