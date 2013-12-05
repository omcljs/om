(ns reactjs.core
  (:require React))

(def Pure
  (React/createClass
    (js-obj
      "shouldComponentUpdate"
      (fn [next-props next-state]
        (this-as this
          (not (= (.. this -props -value) (.-value next-props)))))
      "render"
      (fn []
        (this-as this
          ((.. this -props -children)))))))
