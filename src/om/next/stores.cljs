(ns om.next.stores
  (:require [goog.object :as gobj]
            [om.next.protocols :as p]))

(defn tree-pull [m sel]
  (loop [sel (seq sel) ret {}]
    (if sel
      (let [k (first sel)]
        (cond
          (and (symbol? k) (= k '*)) m

          (keyword? k)
          (if (contains? m k)
            (recur (next sel) (assoc ret k (get m k)))
            (throw
              (ex-info (str "Invalid key " k)
                {:type :error/invalid-key})))

          (map? k)
          (recur (next sel)
            (let [[k sel] (first k)]
              (if (contains? m k)
                (let [v  (get m k)
                      pv (cond
                           (vector? v) (into [] (map #(tree-pull % sel)) v)
                           (map? v) (tree-pull v sel)
                           :else (throw
                                   (ex-info (str "Cannot pull " v)
                                     {:type :error/invalid-tree-pull-value})))]
                  (assoc ret k pv))
                :else
                (throw
                  (ex-info (str "Invalid key " k)
                    {:type :error/invalid-key})))))

          :else
          (throw
            (ex-info (str "Invalid selector fragment " k)
              {:type :error/invalid-selector-fragment}))))
      ret)))

(deftype TreeStore [data index]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (print-map data pr-writer writer opts))
  p/IPull
  (pull [_ selector _]
    (tree-pull data selector))
  p/IPush
  (push [_ entity ctxt]
    (TreeStore. (assoc-in data ctxt entity) index))
  p/IStore
  (commit [this component entity]
    (let [key  (.. component -props -key)
          path (cond->
                 (conj (get-in index [:component->path (type component)]))
                 key (conj key))]
      (set! (.. component -props -omcljs$value) entity)
      [(p/push this entity path) [component]])))

(comment
  (TreeStore. {:foo 1 :bar {:woz 2 :noz 3}} nil)
  (p/pull (TreeStore. {:foo 1 :bar {:woz 2 :noz 3}} nil) [:foo {:bar [:noz]}] nil)
  (p/push (TreeStore. {:foo 1 :bar {:woz 2 :noz 3}} nil) 3 [:bar :woz])
  )

(defn table-pull [m selector ctxt])

(deftype TableStore [data ]
  p/IPush
  (push [this selector ctxt]
    (table-pull this selector ctxt)))

(deftype RemoteStore [data fetch local-keys]
  p/IPushAsync
  (push-async [this entity ctxt cb]))
