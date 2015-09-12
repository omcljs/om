(ns om.next.router)

(defn router [{:keys [read join call]}]
  (fn self [req sel]
    (letfn [(step [[resp next] sel]
              (cond
                ;; lookup
                (keyword? sel) (let [ret (read req sel)]
                                 (if-not (= :om.next/skip ret)
                                   [(assoc resp sel ret) next]
                                   [resp (conj next sel)]))

                ;; invoke
                (seq? sel)     (let [[name params] sel
                                     ret           (call req name params)]
                                 (if-not (= :om.next/skip ret)
                                   [(assoc resp sel ret) next]
                                   [resp (conj next sel)]))

                ;; join
                (map? sel)     (let [[k' sel']     (first sel)
                                     [resp' next'] (join req k' sel')]
                               (if-not (= :om.next/skip resp')
                                 [(assoc resp k' resp')
                                  (if next'
                                    (conj next' {k' next'})
                                    next)]
                                 [resp (conj next sel)]))

                :else          (throw
                                 (ex-info (str "Invalid routing expression " sel)
                                   {:type :error/invalid-routing-expression}))))]
      (reduce step [{} []] sel))))

(comment
  (def todos
    [{:db/id 0 :todo/title "Walk dog" :todo/completed false}
     {:db/id 1 :todo/title "Get milk" :todo/completed false}])

  (defmulti read (fn [_ k] k))

  (defmethod read :todos/count
    [req _] (count todos))

  (defmethod read :todos/user-icon
    [req _] :om.next/skip)

  (defmulti join (fn [_ k _] k))

  (defmethod join :todos/list
    [req _ sel]
    [(into [] (map #(select-keys % sel)) todos) nil])

  (def r
    (router {:read read
             :join join}))

  (r {} [:todos/count :todos/user-icon])

  (r {} [:todos/count :todos/user-icon {:todos/list [:todo/title]}])
  )