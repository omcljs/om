(ns om.next.parser
  (:require [om.next.impl.parser :as parser]))

(defn parser [opts]
  (parser/parser opts))
