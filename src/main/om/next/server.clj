(ns om.next.server
  (:require [om.next.impl.parser :as parser]))

(defn parser [opts]
  (parser/parser (assoc opts :elide-paths true)))
