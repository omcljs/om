(ns examples.harmful.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:import [goog.ui IdGenerator]))

(enable-console-print!)

;; =============================================================================
;; Different backing React class

(defn get-gstate [owner]
  (aget (.-props owner) "__om_app_state"))

(defn merge-pending-state [owner]
  (let [gstate (get-gstate owner)
        spath  [:state-map (om/id owner)]
        states (get-in @gstate spath)]
    (when (:pending-state states)
      (swap! gstate update-in spath
        (fn [states]
          (-> states
            (assoc :render-state
              (merge (:render-state states) (:pending-state states)))
            (dissoc :pending-state)))))))

(def no-local-state-meths
  (assoc om/pure-methods
    :getInitialState
    (fn []
      (this-as this
        (let [c      (om/children this)
              props  (.-props this)
              istate (or (aget props "__om_init_state") {})
              id     (or (::om/id istate)
                         (.getNextUniqueId (.getInstance IdGenerator)))
              state  (merge (dissoc istate ::om/id)
                       (when (satisfies? om/IInitState c)
                         (om/allow-reads (om/init-state c))))
              spath  [:state-map id :render-state]]
          (aset props "__om_init_state" nil)
          (swap! (get-gstate this) assoc-in spath state)
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
        (let [c     (om/children this)
              spath [:state-map (om/id this)]]
          (when (satisfies? om/IWillUnmount c)
            (om/allow-reads (om/will-unmount c)))
          (swap! (get-gstate this) update-in spath dissoc))))
    :componentDidUpdate
    (fn [prev-props prev-state]
      (this-as this
        (let [c      (om/children this)
              gstate (get-gstate this)
              states (get-in @gstate [:state-map (om/id this)])
              spath  [:state-map (om/id this)]]
          (when (satisfies? om/IDidUpdate c)
            (let [state (.-state this)]
              (om/allow-reads
                (om/did-update c
                  (om/get-props #js {:props prev-props})
                  (or (:previous-state states)
                      (:render-state states))))))
          (when (:previous-state states)
            (swap! gstate update-in spath dissoc :previous-state)))))))

(def no-local-descriptor
  (specify! (clj->js no-local-state-meths)
    om/ISetState
    (-set-state!
      ([this val render]
         (om/allow-reads
           (let [props     (.-props this)
                 app-state (aget props "__om_app_state")
                 spath     [:state-map (om/id this) :pending-state]]
             (swap! (get-gstate this) assoc-in spath val)
             (when (and (not (nil? app-state)) render)
               (om/-queue-render! app-state this)))))
      ([this ks val render]
         (om/allow-reads
           (let [props     (.-props this)
                 app-state (aget props "__om_app_state")
                 spath     [:state-map (om/id this) :pending-state]]
             (swap! (get-gstate this) update-in spath assoc-in ks val)
             (when (and (not (nil? app-state)) render)
               (om/-queue-render! app-state this))))))
    om/IGetRenderState
    (-get-render-state
      ([this]
         (let [spath [:state-map (om/id this) :render-state]]
           (get-in @(get-gstate this) spath)))
      ([this ks]
         (get-in (om/-get-render-state this) ks)))
    om/IGetState
    (-get-state
      ([this]
         (let [spath  [:state-map (om/id this)]
               states (get-in @(get-gstate this) spath)]
           (or (:pending-state states)
             (:render-state states))))
      ([this ks]
         (get-in (om/-get-state this) ks)))))

;; =============================================================================
;; Application

(def app-state (atom {:title "A Counter!"}))

(defn counter-view [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:count 0})
    om/IRenderState 
    (render-state [_ {:keys [count]}]
      (dom/div nil
        (dom/h2 nil (:title data))
        (dom/div
          #js {:onClick (fn [e] (om/set-state! owner :count (inc count)))}
          count)))))

(defn label-style []
  #js {:style #js {:width "60px" :display "inline-block"}})

(defn debug-view [[f cursor opts] owner]
  (reify
    om/IInitState
    (init-state [_]
      {:id (.getNextUniqueId (.getInstance IdGenerator))})
    om/IDidMount
    (did-mount [_]
      (om/update-state! owner :id identity))
    om/IRenderState
    (render-state [_ {:keys [id]}]
      (dom/div nil
        (dom/div nil
          (dom/label (label-style) "Props:")
          (dom/code nil (pr-str (om/value cursor))))
        (dom/div nil
          (dom/label (label-style) "State:")
          (dom/code nil
            (pr-str (get-in @(om/state cursor) [:state-map id :render-state]))))
        (om/build* f cursor
          (assoc opts :init-state {::om/id id}))))))

(om/root
  (fn [app owner]
    (om/component
      (om/build counter-view app {:descriptor no-local-descriptor})))
  app-state
  {:target (.getElementById js/document "app")
   :instrument
   (fn [f cursor opts]
     (if (= f counter-view)
       (om/build* debug-view [f cursor opts])
       ::om/pass))})

