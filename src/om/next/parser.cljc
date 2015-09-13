(ns om.next.parser)

(defn parse-prop [prop res quoted env sel]
  (let [ret (prop env sel)]
    (cond
      (contains? ret :value) [(assoc res sel (:value ret)) quoted]
      (contains? ret :quote) [res (conj quoted (:quote ret))]
      :else [res quoted])))

(defn parse-call [call res quoted env sel]
  (let [[name params] sel
        ret (call env name params)]
    (cond
      (contains? ret :value) [(assoc res sel (:value ret)) quoted]
      (contains? ret :quote) [res (conj quoted (:quote ret))]
      :else [res quoted])))

(defn parse-ref [prop res quoted env sel]
  (let [[k' sel'] (first sel)
        ret (prop (assoc env :selector sel') k')]
    [(if-let [[_ value] (find ret :value)]
       (assoc res k' value)
       res)
     (if-let [[_ quoted'] (find ret :quote)]
       (conj quoted {k' quoted'})
       quoted)]))

(defn parser [{:keys [prop call]}]
  (fn self [env sel]
    (let [env (assoc env :parser self)]
      (letfn [(step [[res quoted] sel]
                (cond
                  (keyword? sel) (parse-prop prop res quoted env sel)
                  (seq? sel) (parse-call prop res quoted env sel)
                  (map? sel) (parse-ref prop res quoted env sel)
                  :else (throw
                          (ex-info (str "Invalid routing expression " sel)
                            {:type :error/invalid-routing-expression}))))]
        (reduce step [{} []] sel)))))

(comment
  (def state
    (atom
      {:todos/next-id 2
       :todos/list
       [{:db/id 0 :todo/title "Walk dog" :todo/completed false}
        {:db/id 1 :todo/title "Get milk" :todo/completed false}]}))

  (defmulti prop (fn [_ k] k))

  (defmethod prop :default
    [_ k] {:quote k})

  (defmethod prop :todos/count
    [{:keys [state]} _]
    {:value (count (:todos/list @state))})

  (defmethod prop :todos/list
    [{:keys [state selector]} _]
    {:value (into [] (map #(select-keys % selector))
              (:todos/list @state))
     :quote [:todo/favorites]})

  (defmulti call (fn [_ k _] k))

  (defmethod call 'todos/create
    [{:keys [state]} _ new-todo]
    (swap! state
      (fn [state]
        (let [new-todo (merge new-todo
                         {:db/id (:todos/next-id state)
                          :todo/completed false})]
          (-> state
            (update-in [:todos/list] conj new-todo)
            (update-in [:todos/next-id] inc)))))
    {:value [:todos/list :todos/count]})

  (def p (parser {:prop prop :call call}))

  (p {:state state} [:todos/count :todos/user-icon])

  (p {:state state}
    [:todos/count :todos/user-icon {:todos/list [:todo/title]}])

  (let [new-todo {:todo/title "Pay bills"}]
    (p {:state state} `[(todo/create ~new-todo)]))
  )