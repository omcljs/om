(ns om.next.impl.parser)

(defn parse-prop
  [read res #?@(:clj [quoted?] :cljs [^boolean quoted?])  env sel]
  (let [ret (read env sel {})]
    (if-not quoted?
      (if-let [[_ value] (find ret :value)]
        (assoc res sel value)
        res)
      (if-let [[_ quoted] (find ret :quote)]
        (if (true? quoted)
          (conj res sel)
          (conj res quoted))
        res))))

(defn parse-call
  [read mutate res #?@(:clj [quoted?] :cljs [^boolean quoted?]) env sel]
  (let [[name params]   sel
        [name selector] (if (map? name)
                          (first name)
                          [name nil])
        env             (cond-> env selector
                          (assoc :selector selector))
        mutation?       (symbol? name)
        ret             (if mutation?
                          (mutate env name params)
                          (read env name params))]
    (when (and mutation? (not quoted?))
      (let [action (:action ret)]
        (assert action
          (str name " mutation does not supply :action"))
        (action)))
    (if-not quoted?
      (if-let [[_ value] (find ret :value)]
        (assoc res name value)
        res)
      (if-let [[_ quoted] (find ret :quote)]
        (if (true? quoted)
          (conj res sel)
          (conj res quoted))
        res))))

(defn parse-ref
  [read res #?@(:clj [quoted?] :cljs [^boolean quoted?]) env sel]
  (let [[k' sel'] (first sel)
        ret (read (assoc env :selector sel') k' {})]
    (if-not quoted?
      (if-let [[_ value] (find ret :value)]
        (assoc res k' value)
        res)
      (if-let [[_ quoted] (find ret :quote)]
        (if (true? quoted)
          (conj res sel)
          (conj res {k' quoted}))
        res))))

(defn parser [{:keys [read mutate]}]
  (fn self
    ([env sel] (self env sel false))
    ([env sel #?@(:clj [quoted?] :cljs [^boolean quoted?])]
     (let [env (cond-> (assoc env :parse self)
                 (not (contains? env :path)) (assoc :path [])
                 quoted? (assoc :quoted true))]
       (letfn [(step [res sel]
                 (cond
                   (keyword? sel) (parse-prop read res quoted? env sel)
                   (seq? sel) (parse-call read mutate res quoted? env sel)
                   (map? sel) (parse-ref read res quoted? env sel)
                   :else (throw
                           (ex-info (str "Invalid expression " sel)
                             {:type :error/invalid-expression}))))]
         (reduce step  (if-not quoted? {} []) sel))))))
