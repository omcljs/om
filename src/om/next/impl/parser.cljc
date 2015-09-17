(ns om.next.impl.parser)

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
      (contains? ret :value) [(assoc res name (:value ret)) quoted]
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
    (let [env (cond-> (assoc env :parser self)
                (not (contains? env :path)) (assoc :path []))]
      (letfn [(step [[res quoted] sel]
                (cond
                  (keyword? sel) (parse-prop prop res quoted env sel)
                  (seq? sel) (parse-call call res quoted env sel)
                  (map? sel) (parse-ref prop res quoted env sel)
                  :else (throw
                          (ex-info (str "Invalid expression " sel)
                            {:type :error/invalid-expression}))))]
        (reduce step [{} []] sel)))))

(comment
  (def state
    (atom
      {:todos/next-id 2}))

  (defmulti prop (fn [env k] k))

  (defmethod prop :default
    [env k] {:quote k})

  (defmethod prop :todos/count
    [{:keys [state] :as env} _]
    {:value (count (:todos/list @state))})

  (defmethod prop :todos/list
    [{:keys [state selector]} _]
    (if (contains? @state :todos/list)
      {:value (into [] (map #(select-keys % selector))
                (:todos/list @state))
       :quote [:todo/favorites]}
      {:quote (conj selector :todo/favorites)}))

  (defmulti call (fn [_ k _] k))

  (defmethod call 'todos/create
    [{:keys [state component-ref]} _ new-todo]
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

  (let [new-todo {:todo/title "Pay more bills"}]
    (p {:state state} `[(todos/create ~new-todo)]))

  @state
  )