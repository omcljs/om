(ns om.next
  (:refer-clojure :exclude [var?])
  (:require-macros [om.next :refer [defui]])
  (:require [goog.string :as gstring]
            [clojure.walk :as walk]
            [om.next.protocols :as p]))

(defprotocol IQueryParams
  (-params [this]))

(extend-type default
  IQueryParams
  (-params [_]))

(defprotocol IQuery
  (-query [this]))

(defn var? [x]
  (and (symbol? x)
       (gstring/startsWith (name x) "?")))

(defn var->keyword [x]
  (keyword (.substring (name x) 1)))

(defn bind-query [query params]
  (letfn [(replace-var [node]
            (if (var? node)
              (get params (var->keyword node) node)
              node))]
    (walk/prewalk replace-var query)))

(defn query [cl]
  (bind-query (-query cl) (-params cl)))

(defn create-factory [cl]
  (fn [props children]
    (js/React.createElement cl #js {:omcljs$value props} children)))

(defn props [c]
  (.. c -props -omcljs$value))

(defn root [component store opts]
  (letfn [(render [data]
            (js/React.render component
              data (:target opts)))]
    (let [q (query component)]
      (cond
       (satisfies? p/IRemoteStore store)
       (p/-run-remote-query store q render)
       :else
       (render (p/-run-query store q))))))
