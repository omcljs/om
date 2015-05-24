(ns om.next
  (:refer-clojure :exclude [var?])
  (:require-macros [om.next :refer [defui]])
  (:require [goog.string :as gstring]
            [clojure.walk :as walk]))

(defprotocol IQueryParams
  (-params [this]))

(extend-type default
  IQueryParams
  (-params [_]))

(defprotocol IQuery
  (-queries [this]))

(defprotocol IQueryEngine
  (-run-query [this db q]))

(defprotocol IStorage
  (-transact [this db xs]))

(defn run-query [x db q]
  (-run-query x db q))

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

(defn tree-pull [x selector db fks]
  (loop [selector (seq selector) ret {}]
    (if selector
      (let [k (first selector)]
        (cond
          (keyword? k)
          (recur (next selector) (assoc ret k (get x k)))
          (map? k)
          (recur (next selector)
            (let [[k' selector'] (first k)
                  ys (if (contains? fks k')
                       (let [table (keyword (name k'))]
                         (map (get db table) (get x k')))
                       (get x k'))]
              (assoc ret
                k'
                (vec (map #(tree-pull % selector' db fks) ys)))))))
      ret)))

(defn props [c]
  (bind-props c (.. c -props -omcljs$value)))

(defn create-factory [cl]
  (fn [props children]
    (js/React.createElement cl #js {:omcljs$value props} children)))

(deftype TreeQuery [foreign-keys]
  IQueryEngine
  (-run-query [this db q]
    ))

(deftype TreeStorage []
  IStorage
  (-transact [this db xs]
    ))
