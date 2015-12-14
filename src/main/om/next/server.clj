(ns om.next.server
  (:require [om.next.impl.parser :as parser]
            [om.transit :as transit]))

(defn parser
  "Create a parser. The argument is a map of two keys, :read and :mutate. Both
   functions should have the signature (Env -> Key -> Params -> ParseResult)."
  [opts]
  (parser/parser (assoc opts :elide-paths true)))

(defn dispatch
  "Helper function for implementing :read and :mutate as multimethods. Use this
   as the dispatch-fn."
  [_ key _] key)

(defn reader
  "Create a Om Next transit reader. This reader can handler the tempid type.
   Can pass transit reader customization opts map."
  ([in] (transit/reader in))
  ([in opts] (transit/reader in opts)))

(defn writer
  "Create a Om Next transit reader. This writer can handler the tempid type.
   Can pass transit writer customization opts map."
  ([out] (transit/writer out))
  ([out opts] (transit/writer out opts)))
