(ns om.next.stores
  (:require [om.next.protocols :as p]))

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

(deftype LocalStore [data fks]
  p/IStore
  (-run-query [this qs]
    (tree-pull data qs fks)))

(deftype RemoteStore [data fetch local-keys]
  p/IRemoteStore
  (-run-remote-query [this qs cb]
    (fetch qs cb)))
