(ns om.next.impl.parser)

(declare ->ast)

(defn symbol->ast [k]
  {:dkey k
   :key  k})

(defn keyword->ast [k]
  {:type :prop
   :dkey k
   :key  k})

(defn call->ast [[f args]]
  (if (= 'quote f)
    {:type :remote}
    (let [ast (update-in (->ast f) [:params] merge (or args {}))]
     (cond-> ast
       (symbol? (:dkey ast)) (assoc :type :call)))))

(defn join->ast [join]
  (let [[k v] (first join)
        ast   (->ast k)
        ref?  (vector? (:key ast))]
    (assoc ast :type :prop :sel v)))

(defn ref->ast [[k id :as ref]]
  {:type   :prop
   :dkey   k
   :key    ref
   :params {:id id}})

(defn ->ast [x]
  (cond
    (symbol? x)  (symbol->ast x)
    (keyword? x) (keyword->ast x)
    (map? x)     (join->ast x)
    (vector? x)  (ref->ast x)
    (seq? x)     (call->ast x)
    :else        (throw
                   (ex-info (str "Invalid expression " x)
                     {:type :error/invalid-expression}))))

(defn parser [{:keys [read mutate]}]
  (fn self
    ([env sel] (self env sel false))
    ([env sel #?@(:clj [remote?] :cljs [^boolean remote?])]
     (let [env (cond-> (assoc env :parse self)
                 (not (contains? env :path)) (assoc :path [])
                 remote? (assoc :remote true))]
       (letfn [(step [ret expr]
                 (let [{:keys [key dkey params sel] :as ast} (->ast expr)
                       env   (cond-> env
                               (not (nil? sel)) (assoc :selector sel))
                       type  (:type ast)
                       call? (= :call type)
                       res   (case type
                               :call   (mutate env dkey params)
                               :prop   (read env dkey params)
                               :remote nil)]
                   (if remote?
                     (cond-> ret
                       (true? (:remote res)) (conj expr)
                       (= :remote type)     (conj (second expr)))
                     (if-not (or call? (not= :remote type) (contains? res :value))
                       ret
                       (do
                         (when call?
                           (if-not (nil? (:action res))
                             (do
                               ((:action res))
                               (let [value (:value res)]
                                 (cond-> ret
                                   (not (nil? value)) (assoc key value))))
                             (when-not (true? (:remote res))
                               (throw
                                 (ex-info
                                   (str "Mutation " dkey " does not return an :action")
                                   {:type :error/invalid-mutation})))))
                         (let [value (:value res)]
                           (cond-> ret
                             (not (nil? value)) (assoc key value))))))))]
         (reduce step (if-not remote? {} []) sel))))))

(defn dispatch [_ k _] k)
