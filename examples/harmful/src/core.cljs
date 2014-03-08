(ns examples.harmful.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:import [goog.ui IdGenerator]))

;; =============================================================================
;; Different backing React class

(defn merge-pending-state [owner]
  (swap! (om/state (.-props owner)) [:state-map (om/id owner)]
    (fn [states]
      (-> states
        (assoc :render-state
          (merge (:render-state states) (:pending-state states)))
        (dissoc :pending-state)))))

(def no-local-state-meths
  (assoc om/pure-meths
    :getInitialState
    (fn []
      (this-as this
        (let [c      (om/children this)
              props  (.-props this)
              istate (or (aget props "__om_init_state") {})
              id     (.getNextUniqueId (.getInstance IdGenerator))
              state  (merge istate
                       (when (satisfies? om/IInitState c)
                         (om/allow-reads (om/init-state c))))]
          (aset props "__om_init_state" nil)
          (swap! (om/state props) assoc-in [:state-map id :render-state] state)
          #js {:__om_id id})))
    :componentWillMount
    (fn []
      (this-as this
        (om/merge-props-state this)
        (let [c (om/children this)]
          (when (satisfies? om/IWillMount c)
            (om/allow-reads (om/will-mount c))))
        (merge-pending-state this)))
    :componentWillUnmount
    (fn []
      (this-as this
        (let [c (om/children this)]
          (when (satisfies? om/IWillUnmount c)
            (om/allow-reads (om/will-unmount c)))
          (swap! (om/state (.-props this))
            update-in [:state-map (om/id this)] dissoc))))
    :shouldComponentUpdate
    (fn [next-props next-state]
      (this-as this
        (om/allow-reads
          (let [props (.-props this)
                state (.-state this)
                c     (om/children this)]
            ;; need to merge in props state first
            (om/merge-props-state this next-props)
            (if (satisfies? om/IShouldUpdate c)
              (om/should-update c
                (om/get-props #js {:props next-props})
                (om/-get-state this))
              (cond
                (not (identical? (om/-value (aget props "__om_cursor"))
                                 (om/-value (aget next-props "__om_cursor"))))
                true

                (not (nil? (get-in @(om/state props)
                             [:state-map (om/id this) :pending-state])))
                true

                (not (== (aget props "__om_index") (aget next-props "__om_index")))
                true

                :else false))))))
    :componentWillUpdate
    (fn [next-props next-state]
      (this-as this
        (let [props  (.-props this)
              gstate (om/state props)
              c      (om/children this)]
          (when (satisfies? om/IWillUpdate c)
            (let [state (.-state this)]
              (om/allow-reads
                (om/will-update c
                  (om/get-props #js {:props next-props})
                  (om/-get-state this))))))
        (merge-pending-state this)))
    :componentDidUpdate
    (fn [prev-props prev-state]
      (this-as this
        (let [c      (om/children this)
              gstate (om/state (.-props this))]
          (when (satisfies? om/IDidUpdate c)
            (let [state  (.-state this)
                  states (get-in @gstate [:state-map (om/id this)])]
              (om/allow-reads
                (om/did-update c
                  (om/get-props #js {:props prev-props})
                  (or (:previous-state states)
                      (:render-state states))))))
          (swap! gstate update-in [:state-map (om/id this)] dissoc :previous-state))))))

(def NoLocal
  (js/React.createClass
    (specify! (clj->js no-local-state-meths)
      om/ISetState
      (-set-state!
        ([this val]
           (om/allow-reads
             (let [cursor (aget (.-props this) "__om_cursor")
                   path   (om/-path cursor)]
               (swap! (om/state (.-props this))
                 assoc-in [:state-map (om/id this) :pending-state] val)
               ;; invalidate path to component
               (if (empty? path)
                 (swap! (om/-state cursor) clone)
                 (swap! (om/-state cursor) update-in path clone)))))
        ([this ks val]
           (om/allow-reads
             (let [props  (.-props this)
                   state  (.-state this)
                   cursor (aget props "__om_cursor")
                   path   (om/-path cursor)]
               (swap! (om/state props)
                 update-in [:state-map (om/id this) :pending-state] assoc-in ks val)
               ;; invalidate path to component
               (if (empty? path)
                 (swap! (om/-state cursor) clone)
                 (swap! (om/-state cursor) update-in path clone))))))
      om/IGetRenderState
      (-get-render-state
        ([this]
           (get-in @(om/state (.-props this))
             [:state-map (om/id this) :render-state]))
        ([this ks]
           (get-in (om/-get-render-state this) ks)))
      om/IGetState
      (-get-state
        ([this]
           (let [states (get-in @(om/state (.-props this))
                          [:state-map (om/id this)])]
             (or (:pending-state states)
                 (:render-state states))))
        ([this ks]
           (get-in (om/-get-state this) ks))))))

(defn no-local [obj] (NoLocal. obj))

;; =============================================================================
;; Application

(def app-state (atom {:count 0}))

(defn counter-view [data owner]
  (reify
    om/IRenderState 
    (render [_ {:keys [count]}]
      (dom/div nil
        (dom/h2 nil "A Counter!")
        (dom/div
          #js {:onClick (fn [e] (om/set-state! owner (inc count)))}
          (:count data))))))

(om/root
  (fn [app owner]
    (om/component
      (om/build app {:ctor no-local})))
  app-state
  {:target (.getElementById js/document "app")})

