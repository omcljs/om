(ns om.next.stores)

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

(deftype LocalStore [])

(deftype RemoteStore [fetch local-keys])