(ns reactjs.core
  (:require React))

(def Pure
  (React/createClass
    (js-obj
      "shouldComponentUpdate"
      (fn [next-props next-state]
        (this-as this
          (not (= (.-props this) next-props)))))))
