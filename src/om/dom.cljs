(ns om.dom
  (:refer-clojure :exclude [map meta time])
  (:require-macros [om.dom :as dom]))

(dom/gen-react-dom-fns)

(defn wrap-form-element [ctor]
  (js/React.createClass
    #js
    {:getInitialState
     (fn []
       (this-as this
         #js {:value (.. this -props -value)}))
     :componentWillReceiveProps
     (fn [new-props]
       (this-as this
         (.setState this #js {:value (.-value new-props)})))
     :render
     (fn []
       (this-as this
         (.transferPropsTo
           ;; NOTE: if switch to macro we remove a closure allocation
           (ctor #js {:value (.. this -state -value)}))))}))

(def input (wrap-form-element js/React.DOM.input))

(def textarea (wrap-form-element js/React.DOM.textarea))

(def option (wrap-form-element js/React.DOM.option))

(defn render [component el]
  (js/React.renderComponent component el))
