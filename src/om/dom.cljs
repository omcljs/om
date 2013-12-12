(ns om.dom
  (:refer-clojure :exclude [map meta time])
  (:require-macros [om.dom :as dom])
  (:require React))

(dom/gen-react-dom-fns)

(def ^:dynamic *owner* nil)

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

(defn get-dom-node [owner name]
  (.getDOMNode (aget (.-refs owner) name)))
