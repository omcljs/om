(ns om.next.impl.parser)

(declare expr->ast)

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
    (let [ast (update-in (expr->ast f) [:params] merge (or args {}))]
     (cond-> ast
       (symbol? (:dkey ast)) (assoc :type :call)))))

(defn join->ast [join]
  (let [[k v] (first join)
        ast   (expr->ast k)
        ref?  (vector? (:key ast))]
    (assoc ast :type :prop :sel v)))

(defn ref->ast [[k id :as ref]]
  {:type   :prop
   :dkey   k
   :key    ref
   :params {:om.next/refid id}})

(defn expr->ast [x]
  (cond
    (symbol? x)  (symbol->ast x)
    (keyword? x) (keyword->ast x)
    (map? x)     (join->ast x)
    (vector? x)  (ref->ast x)
    (seq? x)     (call->ast x)
    :else        (throw
                   (ex-info (str "Invalid expression " x)
                     {:type :error/invalid-expression}))))

(defn ast->expr [{:keys [key sel params] :as ast}]
  (let [params (dissoc params :om.next/refid)]
    (if-not (empty? params)
      (list (ast->expr (dissoc ast :params)) params)
      (if-not (nil? sel)
        {key sel}
        key))))

(defn path-meta [x path]
  (let [x' (cond->> x
             (map? x) (into {} (map (fn [[k v]] [k (path-meta v (conj path k))])))
             (vector? x) (into [] (map-indexed #(path-meta %2 (conj path %1)))))]
    (cond-> x'
      #?(:clj  (instance? clojure.lang.IObj x')
         :cljs (satisfies? IWithMeta x'))
      (vary-meta assoc :om-path path))))

(defn parser [{:keys [read mutate] :as config}]
  (fn self
    ([env sel] (self env sel nil))
    ([env sel opts]
     (let [remote? (boolean (:remote opts))
           elide-paths? (boolean (:elide-paths config))
           {:keys [path] :as env}
           (cond-> (assoc env :parse self)
             (not (contains? env :path)) (assoc :path [])
             remote? (assoc :remote true))]
       (letfn [(step [ret expr]
                 (let [{:keys [key dkey params sel] :as ast} (expr->ast expr)
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
                       (let [error (atom nil)]
                         (when (and call? (not (nil? (:action res))))
                           (try
                             ((:action res))
                             #?(:clj  (catch Throwable e
                                        (reset! error e))
                                :cljs (catch :default e
                                        (reset! error e)))))
                         (let [value (:value res)]
                           (cond-> ret
                             @error (assoc key @error)
                             (not (nil? value)) (assoc key value))))))))]
         (cond-> (reduce step (if-not remote? {} []) sel)
           (not (or remote? elide-paths?)) (path-meta path)))))))

(defn dispatch [_ k _] k)
