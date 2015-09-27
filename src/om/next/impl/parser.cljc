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

(defn parse-call-key [key env]
  (if (map? key)
    (let [[k v :as kv] (first key)]
      (if (vector? k)
        [(nth k 0) v (assoc env :id (nth k 1))]
        [k v env]))
    [key nil env]))

(defn parse-call
  [read mutate res #?@(:clj [quoted?] :cljs [^boolean quoted?]) env sel]
  (let [[name params]   sel
        [name sel' env] (parse-call-key name env)
        env             (cond-> env
                          sel' (assoc :selector sel'))
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

(defn parse-join
  [read res #?@(:clj [quoted?] :cljs [^boolean quoted?]) env sel]
  (let [[k' sel'] (first sel)
        [k' env]  (if (vector? k')
                    [(nth k' 0) (assoc env :id (nth k' 1))]
                    [k' env])
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

(declare ->ast)

(defn symbol->ast [k]
  {:dkey k
   :key  k})

(defn keyword->ast [k]
  {:type :prop
   :dkey k
   :key  k})

(defn call->ast [[f args]]
  (let [ast (assoc (->ast f) :params args)]
    (cond-> ast
      (symbol? (:dkey ast)) (assoc :type :call))))

(defn join->ast [join]
  (let [[k v] (first join)
        ast   (->ast k)
        ref?  (= :ref (:type ast k))
        ast   (assoc ast :type :join :sel v)]
    (cond-> ast
      ref? (assoc :type :ref))))

(defn ref->ast [[k id :as ref]]
  {:type :ref
   :dkey k
   :key  ref
   :id   id})

(defn ->ast [x]
  (cond
    (symbol? x)  (symbol->ast x)
    (keyword? x) (keyword->ast x)
    (map? x)     (join->ast x)
    (vector? x)  (ref->ast x)
    (seq? x)     (call->ast x)))

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
                   (map? sel) (parse-join read res quoted? env sel)
                   :else (throw
                           (ex-info (str "Invalid expression " sel)
                             {:type :error/invalid-expression}))))]
         (reduce step  (if-not quoted? {} []) sel))))))
