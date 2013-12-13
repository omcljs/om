(ns om.dom
  (:refer-clojure :exclude [map meta time])
  (:require-macros [om.dom :as dom])
  (:require React))

(dom/gen-react-dom-fns)

(def ^:dynamic *owner* nil)

(defprotocol IWillMount
  (-will-mount [this]))

(defprotocol IDidMount
  (-did-mount [this node]))

(defprotocol IWillUnmount
  (-will-unmount [this]))

(defprotocol IWillUpdate
  (-will-update [this next-props next-state]))

(defprotocol IDidUpdate
  (-did-update [this prev-props prev-state root-node]))

(defprotocol IRender
  (-render [this]))

(def Pure
  (React/createClass
    #js {:shouldComponentUpdate
         (fn [next-props next-state]
           (this-as this
             (not (identical? (.. this -props -value) (.-value next-props)))))
         :componentWillMount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillMount c)
                 (-will-mount c)))))
         :componentDidMount
         (fn [node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidMount c)
                 (-did-mount c node)))))
         :componentWillUnmount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUnmount c)
                 (-will-unmount c)))))
         :componentWillUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUpdate c)
                 (-will-update c next-props next-state)))))
         :componentDidUpdate
         (fn [prev-props prev-state root-node]
           (this-as
             (let [c (.. this -props -children)]
               (when (satisfies? IDidUpdate c)
                 (-will-update c prev-props prev-state root-node)))))
         :render
         (fn []
           (this-as this
             (binding [*owner* this]
               (-render (.. this -props -children)))))}))

(defn render [component el]
  (React/renderComponent component el))

(defn get-node [owner name]
  (.getDOMNode (aget (.-refs owner) name)))
