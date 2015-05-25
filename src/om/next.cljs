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
  (-queries [this]))

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

(defn get-query
  ([cl] (get-query cl :self))
  ([cl k]
   (with-meta
     (bind-query (k (-queries cl)) (k (-params cl)))
     {:class cl})))

(defn queries [cl]
  (letfn [(bind [k]
            (bind-query (k (-queries cl)) (k (-params cl))))
          (key-repeat [[k q]]
            (repeat (count q) k))
          (key-order [bqm]
            (vec (mapcat key-repeat bqm)))]
    (let [qs  (-queries cl)
          qks (keys qs)
          bqs (map bind qks)
          bqm (zipmap qks bqs)]
      (with-meta bqm {:class cl}))))

(defn query-select-keys [q]
  (letfn [(transform [k]
            (cond
              (keyword? k) k
              (map? k) (ffirst k)
              :else (throw (js/Error. (str "Invalid key " k)))))]
    (vec (map transform q))))

;; TODO: handle ?foo/self vars - David
(defn bind-props* [cl props]
  props)

(defn bind-props [c props]
  (bind-props* (type c) props))

(defn props [c]
  (bind-props c (.. c -props -omcljs$value)))

(defn self [c]
  (-> c props :self))

(defn create-factory [cl]
  (fn [props children]
    (js/React.createElement cl #js {:omcljs$value props} children)))

(defn root [component store opts]
  (letfn [(render [data]
            (js/React.render component
              data (:target opts)))]
    (let [qs (queries component)]
      (cond
       (satisfies? p/IRemoteStore store)
       (p/-remote-query store qs render)
       :else
       (render (p/-query store qs))))))
