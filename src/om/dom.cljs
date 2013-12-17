(ns om.dom
  (:refer-clojure :exclude [map meta time])
  (:require-macros [om.dom :as dom]))

(dom/gen-react-dom-fns)

(defn render [component el]
  (js/React.renderComponent component el))
