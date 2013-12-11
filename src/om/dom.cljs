(ns om.dom
  (:refer-clojure :exclude [map meta time])
  (:require-macros [om.dom :as dom])
  (:require React))

(dom/gen-react-dom-fns)

(def Pure
  (React/createClass
    #js {:shouldComponentUpdate
         (fn [next-props next-state]
           (this-as this
             (not (= (.. this -props -value) (.-value next-props)))))
         :render
         (fn []
           (this-as this
             ((.. this -props -children))))}))

