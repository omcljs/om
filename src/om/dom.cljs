(ns om.dom
  (:refer-clojure :exclude [map meta time])
  (:require-macros [om.dom :as dom])
  (:require React))

(dom/gen-react-dom-fns)

(defprotocol IInitState
  (-init-state [this owner]))

(defprotocol IShouldUpdate
  (-should-update [this owner next-props next-state]))

(defprotocol IWillMount
  (-will-mount [this owner]))

(defprotocol IDidMount
  (-did-mount [this owner node]))

(defprotocol IWillUnmount
  (-will-unmount [this owner]))

(defprotocol IWillUpdate
  (-will-update [this owner next-props next-state]))

(defprotocol IDidUpdate
  (-did-update [this owner prev-props prev-state root-node]))

(defprotocol IRender
  (-render [this owner]))

(def Pure
  (React/createClass
    #js {:getInitialState
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               #js {:__om_state
                    (merge {}
                      (when (satisfies? IInitState c)
                        (-init-state c this)))})))

         :shouldComponentUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (if (satisfies? IShouldUpdate c)
                 (-should-update c this next-props next-state)
                 (not (identical? (aget (.-props this) "value")
                                  (aget next-props "value")))))))
         :componentWillMount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillMount c)
                 (-will-mount c this)))))
         :componentDidMount
         (fn [node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidMount c)
                 (-did-mount c this node)))))
         :componentWillUnmount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUnmount c)
                 (-will-unmount c this)))))
         :componentWillUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUpdate c)
                 (-will-update c this next-props next-state)))))
         :componentDidUpdate
         (fn [prev-props prev-state root-node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidUpdate c)
                 (-did-update c this prev-props prev-state root-node)))))
         :render
         (fn []
           (this-as this
             (-render (.. this -props -children) this)))}))

(defn render [component el]
  (React/renderComponent component el))

(defn get-node [owner name]
  (.getDOMNode (aget (.-refs owner) name)))

(defn set-state! [owner k v]
  (.setState owner
    #js {"__om_state" (assoc (aget (.-state owner) "__om_state") k v)}))

(defn get-state [owner k]
  (get (aget (.-state owner) "__om_state") k))
