(ns om.next.router)

(defn update-path [req k]
  (update-in req [:path] (fnil conj []) k))

(defn route [{:keys [read call]}]
  (fn [req expr]
    (letfn [(step [[local remote req] k]
              (cond
                (seq? k)     (let [sym (first k)
                                   ret (call (update-path req k) sym (second k))]
                               (if-not (= :om.next/skip ret)
                                 [(assoc local k ret) remote]
                                 [local (conj remote k)]))
                (map? k)     (let [[k' v] (first k)
                                   ret    (read (update-path req k') k' v)]
                               (if-not (= :om.next/skip ret)
                                 [(assoc local k' ret) remote]
                                 [local (conj remote k)]))
                (keyword? k) (let [ret (read req k)]
                               (if-not (= :om.next/skip ret)
                                 [(assoc local k ret) remote]
                                 [local (conj remote k)]))
                :else        (throw
                               (ex-info (str "Invalid routing expression " k)
                                 {:type :error/invalid-routing-expression}))))]
      (reduce step [{} [] req] expr))))
