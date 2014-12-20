(ns om.impl)

(defn get-props
  [x]
  (aget (.-props x) "__om_cursor"))
