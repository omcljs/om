(ns om.core
  (:require-macros [om.core :refer [pure component]])
  (:require [om.dom :as dom :include-macros true]))

(defprotocol IInitState
  (init-state [this owner]))

(defprotocol IShouldUpdate
  (should-update [this owner next-props next-state]))

(defprotocol IWillMount
  (will-mount [this owner]))

(defprotocol IDidMount
  (did-mount [this owner node]))

(defprotocol IWillUnmount
  (will-unmount [this owner]))

(defprotocol IWillUpdate
  (will-update [this owner next-props next-state]))

(defprotocol IDidUpdate
  (did-update [this owner prev-props prev-state root-node]))

(defprotocol IRender
  (render [this owner]))

(def ^:private Pure
  (js/React.createClass
    #js {:getInitialState
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               #js {:__om_state
                    (merge {}
                      (when (satisfies? IInitState c)
                        (init-state c this)))})))
         :shouldComponentUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (if (satisfies? IShouldUpdate c)
                 (should-update c this next-props next-state)
                 (not (identical? (aget (.-props this) "value")
                                  (aget next-props "value")))))))
         :componentWillMount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillMount c)
                 (will-mount c this)))))
         :componentDidMount
         (fn [node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidMount c)
                 (did-mount c this node)))))
         :componentWillUnmount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUnmount c)
                 (will-unmount c this)))))
         :componentWillUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUpdate c)
                 (will-update c this next-props next-state)))))
         :componentDidUpdate
         (fn [prev-props prev-state root-node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidUpdate c)
                 (did-update c this prev-props prev-state root-node)))))
         :render
         (fn []
           (this-as this
             (render (.. this -props -children) this)))}))

(def refresh-queued false)

(defn root [value f target]
  (let [state (if (instance? Atom value)
                value
                (atom value))
        rootf (fn []
                (set! refresh-queued false)
                (let [path []]
                  (dom/render
                    (pure #js {:value @state}
                      (f (with-meta @state {::state state ::path path})))
                    target)))]
    (add-watch state ::root
      (fn [_ _ _ _]
        (when-not refresh-queued
          (set! refresh-queued true)
          (if (exists? js/requestAnimationFrame)
            (js/requestAnimationFrame rootf)
            (js/setTimeout rootf 16)))))
    (rootf)))

(defn build
  ([f cursor] (build f cursor nil))
  ([f cursor sorm]
    (cond
      (nil? sorm)
      (pure #js {:value cursor} (f cursor))

      (sequential? sorm)
      (let [data    (get-in cursor sorm)
            cursor' (with-meta data (update-in (meta cursor) [::path] into sorm))]
        (pure #js {:value data} (f cursor)))

      :else
      (let [{:keys [path key opts]} sorm
            dataf   (get sorm :fn)
            data    (get-in cursor path)
            data    (if-not (nil? dataf) (dataf data) data)
            rkey    (when-not (nil? key) (get data key))
            cursor' (with-meta data (update-in (meta cursor) [::path] into path))]
        (pure #js {:value data :key rkey}
          (if (nil? opts)
            (f cursor')
            (f cursor' opts)))))))

(defn update!
  ([cursor f]
    (let [m (meta cursor)
          path (::path m)]
      (if (empty? path)
        (swap! (::state m) f)
        (swap! (::state m) update-in path f))))
  ([cursor ks f]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f)))
  ([cursor ks f a]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a)))
  ([cursor ks f a b]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b)))
  ([cursor ks f a b c]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b c)))
  ([cursor ks f a b c d]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b c d)))
  ([cursor ks f a b c d & args]
    (let [m (meta cursor)]
      (apply swap! (::state m) update-in (into (::path m) ks) f a b c d args))))

(defn set-state! [owner ks v]
  (aset (.-state owner) "__om_state"
    (assoc-in (aget (.-state owner) "__om_state") ks v)))

(defn get-state [owner ks]
  (get-in (aget (.-state owner) "__om_state") ks))

(defn get-node [owner name]
  (.getDOMNode (aget (.-refs owner) name)))
