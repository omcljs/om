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
         :render
         (fn []
           (this-as this
             (binding [*owner* this]
               ((.. this -props -children)))))}))

(defn render [component el]
  (React/renderComponent component el))

(defn get-node [owner name]
  (.getDOMNode (aget (.-refs owner) name)))
