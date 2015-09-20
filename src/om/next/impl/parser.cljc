(ns om.next.impl.parser)

;; TODO: unify prop & call

(defn parse-prop [prop res ^boolean quoted? env sel]
  (let [ret (prop env sel)]
    (if-not quoted?
      (if-let [[_ value] (find ret :value)]
        (assoc res sel value)
        res)
      (if-let [[_ quoted] (find ret :quote)]
        (if (true? quoted)
          (conj res sel)
          (conj res quoted))
        res))))

(defn parse-call [call res ^boolean quoted? env sel]
  (let [[name params] sel
        ret (if (and quoted? (symbol? name))
              {:quote true}
              (call env name params))]
    (if-not quoted?
      (if-let [[_ value] (find ret :value)]
        (assoc res name value)
        res)
      (if-let [[_ quoted] (find ret :quote)]
        (if (true? quoted)
          (conj res sel)
          (conj res quoted))
        res))))

(defn parse-ref [prop res ^boolean quoted? env sel]
  (let [[k' sel'] (first sel)
        ret (prop (assoc env :selector sel') k')]
    (if-not quoted?
      (if-let [[_ value] (find ret :value)]
        (assoc res k' value)
        res)
      (if-let [[_ quoted] (find ret :quote)]
        (if (true? quoted)
          (conj res sel)
          (conj res {k' quoted}))
        res))))

(defn parser [{:keys [prop call]}]
  (fn self
    ([env sel] (self env sel false))
    ([env sel ^boolean quoted?]
     (let [env (cond-> (assoc env :parse self)
                 (not (contains? env :path)) (assoc :path [])
                 quoted? (assoc :quoted true))]
       (letfn [(step [res sel]
                 (cond
                   (keyword? sel) (parse-prop prop res quoted? env sel)
                   (seq? sel) (parse-call call res quoted? env sel)
                   (map? sel) (parse-ref prop res quoted? env sel)
                   :else (throw
                           (ex-info (str "Invalid expression " sel)
                             {:type :error/invalid-expression}))))]
         (reduce step  (if-not quoted? {} []) sel))))))
