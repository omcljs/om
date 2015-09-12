(ns om.next.router)

(defn update-path
  ([req k]
   (update-path req k nil))
  ([req k sel]
   (cond-> (update-in req [:path] (fnil conj []) k)
     sel (assoc-in [:selector] sel))))

(defn router [{:keys [read call]}]
  (fn self [req sel]
    (letfn [(step [[resp next] sel]
              (println resp next sel)
              (cond
                (keyword? sel) (let [ret (read req sel)]
                                 (if-not (= :om.next/skip ret)
                                   [(assoc resp sel ret) next]
                                   [resp (conj next sel)]))

                (seq? sel)     (let [[name params] sel
                                     ret           (call req name params)]
                                 (if-not (= :om.next/skip ret)
                                   [(assoc resp sel ret) next]
                                   [resp (conj next sel)]))

                (map? sel)     (let [[k' sel'] (first sel)
                                     ret       (read (update-path req k' sel') k')]
                               (if-not (= :om.next/skip ret)
                                 [(assoc resp k' ret) next]
                                 [resp (conj next sel)]))

                :else          (throw
                                 (ex-info (str "Invalid routing expression " sel)
                                   {:type :error/invalid-routing-expression}))))]
      (reduce step [{} []] sel))))

(comment
  (update-path {} :foo)
  (update-path {} :foo [:bar :baz])

  (def todos
    [{:db/id 0 :todo/title "Walk dog" :todo/completed false}
     {:db/id 1 :todo/title "Get milk" :todo/completed false}])

  (defmulti read (fn [_ sel] sel))

  (defmethod read :todos/count
    [req _] (count todos))

  (defmethod read :todos/user-icon
    [req _] :om.next/skip)

  (def r (router {:read read}))

  (r {} [:todos/count :todos/user-icon])
  )