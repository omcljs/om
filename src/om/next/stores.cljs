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
                (throw
                  (ex-info (str "Invalid key " k)
                    {:type :error/invalid-key})))))
          :else
          (throw
            (ex-info (str "Invalid selector fragment " k)
              {:type :error/invalid-selector-fragment}))))
      ret)))

(deftype TreeStore [data]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (print-map data pr-writer writer opts))
  p/IStore
  p/IPull
  (pull [_ selector _]
    (tree-pull data selector))
  p/IPush
  (push [_ entity ctxt]
    (if (empty? ctxt)
      (TreeStore. entity)
      (TreeStore. (assoc-in data ctxt entity)))))

(comment
  (TreeStore. {:foo 1 :bar {:woz 2 :noz 3}} nil)
  (p/pull (TreeStore. {:foo 1 :bar {:woz 2 :noz 3}} nil) [:foo {:bar [:noz]}] nil)
  (p/push (TreeStore. {:foo 1 :bar {:woz 2 :noz 3}} nil) 3 [:bar :woz])
  )

(defn tables-pull [tables selector]
  (letfn [(tables-pull* [m sel fks]
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
                        (cond
                          (fks k)
                          (let [table (get tables k)
                                v (get m k)]
                            (if (vector? v)
                              (assoc ret
                                k (into []
                                    (map #(tables-pull* (get table %) sel fks))
                                    v))
                              (assoc ret k (tables-pull* v sel fks))))

                          (contains? m k)
                          (let [v (get m k)
                                pv (cond
                                     (vector? v) (into [] (map #(tables-pull* % sel fks)) v)
                                     (map? v) (tables-pull* v sel fks)
                                     :else (throw
                                             (ex-info (str "Cannot pull " v)
                                               {:type :error/invalid-table-pull-value})))]
                            (assoc ret k pv))

                          :else
                          (throw
                            (ex-info (str "Invalid key " k)
                              {:type :error/invalid-key})))))

                    :else
                    (throw
                      (ex-info (str "Invalid selector fragment " k)
                        {:type :error/invalid-selector-fragment}))))
                ret)))]
    (tables-pull* tables selector (set (keys tables)))))

(comment
  (tables-pull
    {:app {:app/title "Hello World!" :app/state [0 1 2]}
     :app/state [{:state/count 0}
                 {:state/count 0}
                 {:state/count 0}]}
    [{:app [:app/title {:app/state [:state/count]}]}])
  )

(deftype TablesStore [data]
  p/IStore
  p/IPull
  (pull [this selector ctxt]
    (tables-pull this selector)))

(deftype RemoteStore [data fetch local-keys]
  p/IPushAsync
  (push-async [this entity ctxt cb]))
