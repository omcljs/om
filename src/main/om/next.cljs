(ns om.next
  (:refer-clojure :exclude [var? key replace])
  (:require-macros [om.next :refer [defui]])
  (:require [goog.string :as gstring]
            [goog.object :as gobj]
            [goog.log :as glog]
            [clojure.walk :as walk]
            [om.next.protocols :as p]
            [om.next.impl.parser :as parser]
            [om.next.cache :as c]
            [clojure.zip :as zip])
  (:import [goog.debug Console]))

(defonce *logger*
  (when ^boolean goog.DEBUG
    (.setCapturing (Console.) true)
    (glog/getLogger "om.next")))

;; =============================================================================
;; Globals & Dynamics

(def ^:private roots (atom {}))
(def ^{:dynamic true} *raf* nil)
(def ^{:dynamic true :private true} *reconciler* nil)
(def ^{:dynamic true :private true} *parent* nil)
(def ^{:dynamic true :private true} *shared* nil)
(def ^{:dynamic true :private true} *instrument* nil)
(def ^{:dynamic true :private true} *depth* 0)

;; =============================================================================
;; Utilities

(defn ^boolean nil-or-map? [x]
  (or (nil? x) (map? x)))

(defn- node->key [node]
  (cond
    (map? node) (ffirst node)
    (seq? node) (let [node' (first node)]
                  (when (map? node')
                    (ffirst node')))
    :else nil))

(defn- query-zip [root]
  (zip/zipper
    #(or (vector? %) (map? %) (seq? %))
    seq
    (fn [node children]
      (let [ret (cond
                  (vector? node) (vec children)
                  (map? node)    (into {} children)
                  (seq? node)    children)]
        (with-meta ret (meta node))))
    root))

(defn- move-to-key [loc k]
  (loop [loc (zip/down loc)]
    (let [node (zip/node loc)]
      (if (= k (first node))
        (-> loc zip/down zip/right)
        (recur (zip/right loc))))))

(defn ^boolean union? [node]
  (and (map? node) (< 1 (count node))))

(defn- query-template [query path]
  (letfn [(query-template* [loc path]
            (if (empty? path)
              loc
              (let [node (zip/node loc)]
                (if (vector? node) ;; SUBQUERY
                  (recur (zip/down loc) path)
                  (let [[k & ks] path]
                    (if (union? node)
                      (let [node (zip/node (move-to-key loc k))]
                        (recur (zip/replace loc node) ks)) ;; UNION
                      (let [k' (node->key node)]
                        (if (keyword-identical? k k')
                          (if (map? node)
                            (recur (move-to-key loc k) ks) ;; JOIN
                            (recur (-> loc zip/down zip/down zip/down zip/right) ks)) ;; CALL
                          (recur (zip/right loc) path)))))))))]
    (query-template* (query-zip query) path)))

(defn- replace [template new-query]
  (-> template (zip/replace new-query) zip/root))

(declare focus-query)

(defn- join-key [node]
  (cond
    (map? node) (ffirst node)
    (seq? node) (join-key (first node))
    :else       node))

(defn- focused-join [node ks]
  (cond
    (map? node) {(ffirst node) (focus-query (-> node first second) ks)}
    (seq? node) (list (focused-join (first node) ks) (second node))
    :else       node))

(defn- focus-query [query path]
  (if (empty? path)
    query
    (let [[k & ks] path]
      (letfn [(match [x]
                (= k (join-key x)))
              (value [x]
                (focused-join x ks))]
        (if (map? query) ;; UNION
          {k (focus-query (get query k) ks) ::union true}
          (into [] (comp (filter match) (map value) (take 1)) query))))))

(defn- focus->path
  ([focus]
   (focus->path focus '* []))
  ([focus bound]
   (focus->path focus bound []))
  ([focus bound path]
   (if (and (or (= bound '*) (not= path bound))
            (some map? focus)
            (== 1 (count focus)))
     (let [[k focus'] (ffirst focus)]
       (recur focus' bound (conj path k)))
     path)))

;; =============================================================================
;; Query Protocols & Helpers

(defprotocol Ident
  (ident [this props] "Return the ref for this component"))

(extend-type default
  Ident
  (ident [this props] this))

(defprotocol IQueryParams
  (params [this] "Return the query parameters"))

(extend-type default
  IQueryParams
  (params [_]))

(defprotocol IQuery
  (query [this] "Return the component's unbound query"))

(defprotocol ILocalState
  (-set-state! [this new-state] "Set the component's local state")
  (-get-state [this] "Get the component's local state")
  (-get-rendered-state [this] "Get the component's rendered local state")
  (-merge-pending-state! [this] "Get the component's pending local state"))

(defn- var? [x]
  (and (symbol? x)
       (gstring/startsWith (name x) "?")))

(defn- var->keyword [x]
  (keyword (.substring (name x) 1)))

(defn- bind-query [query params]
  (letfn [(replace-var [node]
            (if (var? node)
              (get params (var->keyword node) node)
              node))]
    (walk/prewalk replace-var query)))

(declare component? get-reconciler props)

(defn- get-local-query-data [component]
  (some-> (get-reconciler component)
    :config :state deref ::queries (get component)))

(defn get-unbound-query
  "Return the unbound query for a component."
  [component]
  (:query (get-local-query-data component) (query component)))

(defn get-params
  "Return the query params for a component."
  [component]
  (:params (get-local-query-data component) (params component)))

(defn- get-component-query [c]
  (let [qps (get-local-query-data c)
        q   (:query qps (query c))
        c'  (-> q meta :component)]
    (assert (nil? c') (str "Query violation, " c , " reuses " c' " query"))
    (with-meta
      (bind-query q (:params qps (params c)))
      {:component (type c)})))

(defn get-query
  "Return a IQuery/IParams instance bound query. Works for component classes
   and component instances. See also om.next/full-query."
  [x]
  (if (satisfies? IQuery x)
    (if (component? x)
      (get-component-query x)
      (let [q (query x)
            c (-> q meta :component)]
        (assert (nil? c) (str "Query violation, " x , " reuses " c " query"))
        (with-meta (bind-query q (params x)) {:component x})))
    ;; in advanced, statics will get elided
    (when (goog/isFunction x)
      (let [x (js/Object.create (. x -prototype))]
        (when (satisfies? IQuery x)
          (let [q (query x)
                c (-> q meta :component)]
            (assert (nil? c) (str "Query violation, " x , " reuses " c " query"))
            (with-meta (bind-query q (params x)) {:component x})))))))

(defn iquery? [x]
  (satisfies? IQuery x))

(defn tag [x class]
  (vary-meta x assoc :component class))

;; =============================================================================
;; React Bridging

(deftype ^:private OmProps [props basis-t])

(defn- om-props [props basis-t]
  (OmProps. props basis-t))

(defn- om-props-basis [om-props]
  (.-basis-t om-props))

(def ^:private nil-props (om-props nil -1))

(defn- unwrap [om-props]
  (.-props om-props))

(defn- compute-react-key [cl props]
  (if-let [rk (:react-key props)]
    rk
    (if-let [idx (:om-path props)]
      (str (. cl -name) "_" idx)
      js/undefined)))

(defn factory
  "Create a factory constructor from a component class created with
   om.next/defui."
  ([class] (factory class nil))
  ([class {:keys [validator keyfn] :as opts}]
   {:pre [(fn? class)]}
   (fn [props & children]
     (when-not (nil? validator)
       (assert (validator props)))
     (if *instrument*
       (apply *instrument* props children)
       (let [key (if-not (nil? keyfn)
                   (keyfn props)
                   (compute-react-key class props))
             ref (:ref props)
             ref (cond-> ref (keyword? ref) str)
             t   (if-not (nil? *reconciler*)
                   (p/basis-t *reconciler*)
                   0)]
         (js/React.createElement class
          #js {:key key
               :ref ref
               :omcljs$value      (om-props props t)
               :omcljs$path       (-> props meta :om-path)
               :omcljs$reconciler *reconciler*
               :omcljs$parent     *parent*
               :omcljs$shared     *shared*
               :omcljs$instrument *instrument*
               :omcljs$depth      *depth*}
          children))))))

(defn ^boolean component?
  "Returns true if the argument is an Om component."
  [x]
  (. x -om$isComponent))

(defn- state [c]
  {:pre [(component? c)]}
  (.-state c))

(defn- get-prop
  "PRIVATE: Do not use"
  [c k]
  (gobj/get (.-props c) k))

(defn- get-props*
  [x k]
  (if (nil? x)
    nil-props
    (let [y (gobj/get x k)]
      (if (nil? y)
        nil-props
        y))))

(defn- get-prev-props [x]
  (get-props* x "omcljs$prev$value"))

(defn- get-next-props [x]
  (get-props* x "omcljs$next$value"))

(defn- get-props [x]
  (get-props* x "omcljs$value"))

(defn- set-prop!
  "PRIVATE: Do not use"
  [c k v]
  (gobj/set (.-props c) k v))

(defn- get-reconciler
  [c]
  {:pre [(component? c)]}
  (get-prop c "omcljs$reconciler"))

(defn- props*
  ([x y]
   (max-key om-props-basis x y))
  ([x y z]
   (max-key om-props-basis x (props* y z))))

(defn- prev-props*
  ([x y]
   (min-key om-props-basis x y))
  ([x y z]
   (min-key om-props-basis
     (props* x y) (props* y z))))

(defn -prev-props [prev-props component]
  (let [cst   (.-state component)
        props (.-props component)]
    (unwrap
      (prev-props*
        (props* (get-props prev-props) (get-prev-props cst))
        (props* (get-props cst) (get-props props))))))

(defn -next-props [next-props component]
  (unwrap
    (props*
      (get-props next-props)
      (-> component .-props get-props)
      (-> component .-state get-next-props))))

(defn- merge-pending-props! [c]
  {:pre [(component? c)]}
  (let [cst     (. c -state)
        props   (.-props c)
        pending (gobj/get cst "omcljs$next$value")
        prev    (props* (get-props cst) (get-props props))]
    (gobj/set cst "omcljs$prev$value" prev)
    (when-not (nil? pending)
      (gobj/remove cst "omcljs$next$value")
      (gobj/set cst "omcljs$value" pending))))

(defn- clear-prev-props! [c]
  (gobj/remove (.-state c) "omcljs$prev$value"))

(defn- t
  "Get basis t value for when the component last read its props from
   the global state."
  [c]
  (om-props-basis
    (props*
      (-> c .-props get-props)
      (-> c .-state get-props))))

(defn- parent
  "Returns the parent Om component."
  [component]
  (get-prop component "omcljs$parent"))

(defn- depth
  "PRIVATE: Returns the render depth (a integer) of the component relative to
  the mount root."
  [component]
  (get-prop component "omcljs$depth"))

(defn react-key
  "Returns the components React key."
  [component]
  (.. component -props -key))

(defn react-type
  "Returns the component type, regardless of whether the component has been
   mounted"
  [x]
  (or (gobj/get x "type") (type x)))

(defn- path
  "Returns the component's Om data path."
  [c]
  (get-prop c "omcljs$path"))

(defn shared [component]
  {:pre [(component? component)]}
  (get-prop component "omcljs$shared"))

(defn instrument [component]
  {:pre [(component? component)]}
  (get-prop component "omcljs$instrument"))

(defn- update-props! [c next-props]
  {:pre [(component? c)]}
  ;; We cannot write directly to props, React will complain
  (doto (.-state c)
    (gobj/set "omcljs$next$value"
      (om-props next-props (p/basis-t (get-reconciler c))))))

(defn props
  "Return a components props."
  [component]
  {:pre [(component? component)]}
  ;; When force updating we write temporarily props into state to avoid bogus
  ;; complaints from React. We record the basis T of the reconciler to determine
  ;; if the props recorded into state are more recent - props will get updated
  ;; when React actually re-renders the component.
  (unwrap
    (props*
      (-> component .-props get-props)
      (-> component .-state get-props))))

(defn computed
  "Add computed properties to props."
  [props computed-map]
  (if (vector? props)
    (cond-> props
      (not (empty? computed-map)) (vary-meta assoc :om.next/computed computed-map))
    (cond-> props
      (not (empty? computed-map)) (assoc :om.next/computed computed-map))))

(defn get-computed
  "Return the computed properties on a component or its props."
  ([x]
   (get-computed x []))
  ([x k-or-ks]
   (let [props (cond-> x (component? x) props)
         ks    (into [:om.next/computed]
                 (cond-> k-or-ks
                   (not (sequential? k-or-ks)) vector))]
     (if (vector? props)
       (-> props meta (get-in ks))
       (get-in props ks)))))

(defn get-ident
  "Given a component return its ident"
  [component]
  {:pre [(component? component)]}
  (ident component (props component)))

(declare schedule-render!)

(defn set-state!
  "Set the component local state of the component. Analogous to React's
   setState."
  [component new-state]
  {:pre [(component? component)]}
  (if (satisfies? ILocalState component)
    (-set-state! component new-state)
    (gobj/set (.-state component) "omcljs$pendingState" new-state))
  (if-let [r (get-reconciler component)]
    (do
      (p/queue! r [component])
      (schedule-render! r))
    (.forceUpdate component)))

(defn get-state
  "Get a component's local state. May provide a single key or a sequential
   collection of keys for indexed access into the component's local state."
  ([component]
   (get-state component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [cst (if (satisfies? ILocalState component)
               (-get-state component)
               (when-let [state (. component -state)]
                 (or (gobj/get state "omcljs$pendingState")
                     (gobj/get state "omcljs$state"))))]
     (get-in cst (if (sequential? k-or-ks) k-or-ks [k-or-ks])))))

(defn update-state!
  "Update a component's local state. Similar to Clojure(Script)'s update-in."
  ([component f]
   (set-state! component (f (get-state component))))
  ([component f arg0]
   (set-state! component (f (get-state component) arg0)))
  ([component f arg0 arg1]
   (set-state! component (f (get-state component) arg0 arg1)))
  ([component f arg0 arg1 arg2]
   (set-state! component (f (get-state component) arg0 arg1 arg2)))
  ([component f arg0 arg1 arg2 arg3]
   (set-state! component (f (get-state component) arg0 arg1 arg2 arg3)))
  ([component f arg0 arg1 arg2 arg3 & arg-rest]
   (set-state! component
     (apply f (get-state component) arg0 arg1 arg2 arg3 arg-rest))))

(defn get-rendered-state
  "Get the rendered state of component. om.next/get-state always returns the
   up-to-date state."
  [component]
  {:pre [(component? component)]}
  (if (satisfies? ILocalState component)
    (-get-rendered-state component)
    (some-> component .-state (gobj/get "omcljs$state"))))

(defn- merge-pending-state! [c]
  (if (satisfies? ILocalState c)
    (-merge-pending-state! c)
    (when-let [pending (some-> c .-state (gobj/get "omcljs$pendingState"))]
      (let [state    (.-state c)
            previous (gobj/get state "omcljs$state")]
        (gobj/remove state "omcljs$pendingState")
        (gobj/set state "omcljs$previousState" previous)
        (gobj/set state "omcljs$state" pending)))))

(defn react-set-state!
  ([component new-state]
   (react-set-state! component new-state nil))
  ([component new-state cb]
   {:pre [(component? component)]}
   (.setState component #js {:omcljs$state new-state} nil)))

;; TODO: will need to reindex

(defn set-query!
  "Change the query of a component. Will schedule a re-render."
  [component new-query]
  {:pre [(component? component)]}
  (let [r   (get-reconciler component)
        cfg (:config r)
        st  (:state cfg)
        id  (random-uuid)
        _   (.add (:history cfg) id @st)]
    (when-not (nil? *logger*)
      (glog/info *logger*
        (str (when-let [ref (ident component (props component))]
               (str (pr-str ref) " "))
          "changed query '" new-query ", " (pr-str id))))
    (swap! st update-in [:om.next/queries component] merge {:query new-query})
    (p/queue! r [component])
    (p/reindex! r)
    nil))

(defn set-params!
  "Change the query parameters of a component. Will schedule a re-render."
  [component new-params]
  {:pre [(component? component)]}
  (let [r   (get-reconciler component)
        cfg (:config r)
        st  (:state cfg)
        id  (random-uuid)
        _   (.add (:history cfg) id @st)]
    (when-not (nil? *logger*)
      (glog/info *logger*
        (str (when-let [ref (ident component (props component))]
               (str (pr-str ref) " "))
          "changed query params " new-params", " (pr-str id))))
    (swap! st update-in [:om.next/queries component] merge {:params new-params})
    (p/queue! r [component])
    (p/reindex! r)
    nil))

(defn ^boolean mounted?
  "Returns true if the component is mounted."
  [x]
  (and (component? x) ^boolean (.isMounted x)))

(defn react-ref
  "Returns the component associated with a component's React ref."
  [component name]
  (some-> (.-refs component) (gobj/get name)))

(defn children
  "Returns the component's children."
  [component]
  (.. component -props -children))

(defn- update-component! [c next-props]
  {:pre [(component? c)]}
  (update-props! c next-props)
  (.forceUpdate c))

(defn should-update?
  ([c next-props]
   (should-update? c next-props nil))
  ([c next-props next-state]
   {:pre [(component? c)]}
   (.shouldComponentUpdate c
     #js {:omcljs$value next-props}
     #js {:omcljs$state next-state})))

(defn- class-path [c]
  {:pre [(component? c)]}
  (loop [c c ret (list (type c))]
    (if-let [p (parent c)]
      (if (iquery? p)
        (recur p (cons (type p) ret))
        (recur p ret))
      (let [seen (atom #{})]
        (take-while
          (fn [x]
            (when-not (contains? @seen x)
              (swap! seen conj x)
              x))
          ret)))))

(defn- join-value [node]
  (if (seq? node)
    (ffirst node)
    (first node)))

(defn subquery
  "Given a class or mounted component x and a ref to an instantiated component
   or class that defines a subquery, pick the most specific subquery. If the
   component is mounted subquery-ref will be used, subquery-class otherwise."
  [x subquery-ref subquery-class]
  {:pre [(or (keyword? subquery-ref) (string? subquery-ref))
         (fn? subquery-class)]}
  (let [ref (cond-> subquery-ref (keyword? subquery-ref) str)]
    (if (and (component? x) (mounted? x))
      (get-query (react-ref x ref))
      (get-query subquery-class))))

;; =============================================================================
;; Reconciler API

(declare reconciler?)

(defn- basis-t [reconciler]
  (p/basis-t reconciler))

(defn schedule-render! [reconciler]
  (when (p/schedule-render! reconciler)
    (let [f #(p/reconcile! reconciler)]
      (cond
        (fn? *raf*) (*raf* f)

        (not (exists? js/requestAnimationFrame))
        (js/setTimeout f 16)

        :else
        (js/requestAnimationFrame f)))))

(defn schedule-sends! [reconciler]
  (when (p/schedule-sends! reconciler)
    (js/setTimeout #(p/send! reconciler) 300)))

(declare remove-root!)

(defn add-root!
  "Given a root component class and a target root DOM node, instantiate and
   render the root class using the reconciler's :state property. The reconciler
   will continue to observe changes to :state and keep the target node in sync.
   Note a reconciler may have only one root. If invoked on a reconciler with an
   existing root, the new root will replace the old one."
  ([reconciler root-class target]
   (add-root! reconciler root-class target nil))
  ([reconciler root-class target options]
   {:pre [(reconciler? reconciler) (fn? root-class)]}
   (when-let [old-reconciler (get @roots target)]
     (remove-root! old-reconciler target))
   (swap! roots assoc target reconciler)
   (p/add-root! reconciler root-class target options)))

(defn remove-root!
  "Remove a root target (a DOM element) from a reconciler. The reconciler will
   no longer attempt to reconcile application state with the specified root."
  [reconciler target]
  (p/remove-root! reconciler target))

;; =============================================================================
;; Transactions

(defprotocol ITxIntercept
  (tx-intercept [c tx]
    "An optional protocol that component may implement to intercept child
     transactions."))

(defn- to-env [x]
  (let [config (if (reconciler? x) (:config x) x)]
    (select-keys config [:state :shared :parser])))

(defn gather-sends
  [{:keys [parser] :as env} tx remotes]
  (into {}
    (comp
      (map #(vector % (parser env tx %)))
      (filter (fn [[_ v]] (pos? (count v)))))
    remotes))

(defn transact* [r c ref tx]
  (let [cfg  (:config r)
        ref  (if (and c (not ref))
               (ident c (props c))
               ref)
        env  (merge
               (to-env cfg)
               {:reconciler r :component c}
               (when ref
                 {:ref ref}))
        id   (random-uuid)
        _    (.add (:history cfg) id @(:state cfg))
        _    (when-not (nil? *logger*)
               (glog/info *logger*
                 (str (when ref (str (pr-str ref) " "))
                   "transacted '" tx ", " (pr-str id))))
        v    ((:parser cfg) env tx)
        snds (gather-sends env tx (:remotes cfg))]
    (p/queue! r
      (into (if ref [ref] [])
        (remove symbol? (keys v))))
    (when-not (empty? snds)
      (p/queue-sends! r snds)
      (schedule-sends! r))))

(defn transact!
  "Given a reconciler or component run a transaction. tx is a parse expression
   that should include mutations followed by any necessary read. The reads will
   be used to trigger component re-rendering. If given a reconciler can be
   optionally passed a ref as the second argument.

   Example:

     (om/transact! widget
       '[(do/this!) (do/that!)
         :read/this :read/that])"
  ([x tx]
   {:pre [(vector? tx)]}
   (if (reconciler? x)
     (transact* x nil nil tx)
     (do
       (assert (satisfies? IQuery x)
         (str "transact! invoked by component " x
              " that does not implement IQuery"))
       (loop [p (parent x) tx tx]
         (if (nil? p)
           (transact* (get-reconciler x) x nil tx)
           (let [tx (if (satisfies? ITxIntercept p)
                      (tx-intercept p tx)
                      tx)]
             (recur (parent p) tx)))))))
  ([r ref tx]
   (transact* r nil ref tx)))

;; =============================================================================
;; Parser

(defn parser
  "Create a parser. The argument is a map of two keys, :read and :mutate. Both
   functions should have the signature (Env -> Key -> Params -> ParseResult)."
  [{:keys [read mutate] :as opts}]
  {:pre [(map? opts)]}
  (parser/parser opts))

(defn dispatch
  "Helper function for implementing :read and :mutate as multimethods. Use this
   as the dispatch-fn."
  [_ key _] key)

;; =============================================================================
;; Indexer

(defn- join? [x]
  (let [x (if (seq? x) (first x) x)]
    (map? x)))

(defrecord Indexer [indexes]
  IDeref
  (-deref [_] @indexes)

  p/IIndexer
  (index-root [_ x]
    (let [prop->classes     (atom {})
          class-path->query (atom {})
          rootq             (get-query x)
          class             (cond-> x (component? x) type)]
      (letfn [(build-index* [class selector path classpath]
                (when class
                  (swap! class-path->query update-in [classpath]
                    (fnil conj #{})
                    (query-template (focus-query rootq path) path)))
                (if (vector? selector)
                  (let [{props false joins true} (group-by join? selector)]
                    (when class
                      (swap! prop->classes
                        #(merge-with into % (zipmap props (repeat #{class})))))
                    (doseq [join joins]
                      (let [[prop selector'] (join-value join)]
                        (when class
                          (swap! prop->classes
                            #(merge-with into % {prop #{class}})))
                        (let [class' (-> selector' meta :component)]
                          (build-index* class' selector'
                            (conj path prop)
                            (cond-> classpath class' (conj class')))))))
                  ;; Map case, union in query
                  (doseq [[k selector'] selector]
                    (let [class' (-> selector' meta :component)]
                      (build-index* class' selector' (conj path k)
                        (cond-> classpath class' (conj class')))))))]
        (build-index* class rootq [] [class])
        (swap! indexes merge
          {:prop->classes     @prop->classes
           :class-path->query @class-path->query}))))

  (index-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in indexes
                        [:class->components (type c)]
                        (fnil conj #{}) c)
              ref     (ident c (props c))]
          (if-not (component? ref)
            (cond-> indexes
              ref (update-in [:ref->components ref] (fnil conj #{}) c))
            indexes)))))

  (drop-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in indexes
                        [:class->components (type c)]
                        disj c)
              ref     (ident c (props c))]
          (if-not (component? ref)
            (cond-> indexes
              ref (update-in [:ref->components ref] disj c))
            indexes)))))

  (key->components [_ k]
    (let [indexes @indexes]
      (if (component? k)
        #{k}
        (let [cs (get-in indexes [:ref->components k] ::not-found)]
          (if-not (keyword-identical? ::not-found cs)
            cs
            (if (keyword? k)
              ;; TODO: more robust validation, might be bogus key
              (let [cs (get-in indexes [:prop->classes k])]
                (transduce (map #(get-in indexes [:class->components %]))
                  (completing into) #{} cs))
              (throw (js/Error. (str "Invalid key " k ", key must be ref or keyword"))))))))))

(defn indexer
  "Given a function (Component -> Ref), return an indexer."
  []
  (Indexer.
    (atom
      {:class->components {}
       :ref->components   {}})))

(defn get-indexer
  "PRIVATE: Get the indexer associated with the reconciler."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :indexer))

(defn ref->components
  "Return all components for a given ref."
  [x ref]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (p/key->components indexer ref)))

(defn ref->any
  "Get any component from the indexer that matches the ref."
  [x ref]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (first (p/key->components indexer ref))))

(defn class->any
  "Get any component from the indexer that matches the component class."
  [x class]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (first (get-in @indexer [:class->components class]))))

(defn class-path->query
  [x y]
  (let [indexer (if (reconciler? x) (get-indexer x) x)
        cp      (if (component? y) (class-path y) y)]
    (into #{} (map zip/root)
      (get-in @indexer [:class-path->query cp]))))

(defn full-query
  "Returns the absolute query for a given component, not relative like
   om.next/get-query."
  ([component]
   (when (satisfies? IQuery component)
     (replace
       (first
         (get-in @(-> component get-reconciler get-indexer)
           [:class-path->query (class-path component)]))
       (get-query component))))
  ([component path]
   (when (satisfies? IQuery component)
     (let [path' (into [] (remove number?) path)
           cp    (class-path component)
           qs    (get-in @(-> component get-reconciler get-indexer)
                   [:class-path->query cp])]
       (if-not (empty? qs)
         ;; handle case where child appears multiple times at same class-path
         ;; but with different queries
         (let [q (first (filter #(= path' (-> % zip/root (focus->path path'))) qs))]
           (if-not (nil? q)
             (replace q (get-query component))
             (throw
               (ex-info (str "No queries exist for component path " cp " or data path " path')
                 {:type :om.next/no-queries}))))
         (throw
           (ex-info (str "No queries exist for component path " cp)
             {:type :om.next/no-queries})))))))

;; for advanced optimizations
(defn to-class [class]
  (if (not (satisfies? Ident class))
    (js/Object.create (. class -prototype))
    class))

(defn- normalize* [selector data refs]
  (cond
    (= '[*] selector) data

    ;; union case
    (map? selector)
    (let [class (to-class (-> selector meta :component))
          ref   (ident class data)]
      (normalize* (get selector (first ref)) data refs))

    :else
    (loop [q (seq selector) ret {}]
      (if-not (nil? q)
        (let [node (first q)]
          (if (join? node)
            (let [[k sel] (join-value node)
                  class   (to-class (-> sel meta :component))
                  v       (get data k)]
              (cond
                ;; normalize one
                (map? v)
                (let [x (normalize* sel v refs)
                      i (ident class v)]
                  (swap! refs update-in [(first i) (second i)] merge x)
                  (recur (next q) (assoc ret k i)))

                ;; normalize many
                (vector? v)
                (let [xs (into [] (map #(normalize* sel % refs)) v)
                      is (into [] (map #(ident class %)) xs)]
                  (if (vector? sel)
                    (swap! refs update-in [(ffirst is)]
                      (fn [ys]
                        (merge-with merge ys
                          (zipmap (map second is) xs))))
                    (swap! refs
                      (fn [refs']
                        (reduce
                          (fn [ret [i x]]
                            (update-in ret i merge x))
                          refs' (map vector is xs)))))
                  (recur (next q) (assoc ret k is)))

                ;; missing key
                (nil? v)
                (recur (next q) ret)

                ;; can't handle
                :else (recur (next q) (assoc ret k v))))
            (let [k (if (seq? node) (first node) node)
                  v (get data k)]
              (if (nil? v)
                (recur (next q) ret)
                (recur (next q) (assoc ret k v))))))
        ret))))

(defn normalize
  "Given a Om component class or instance and some data, use the component's
   query to transform the data into normal form. If merge-ref option is true,
   will return refs in the result instead of as metadata."
  ([x data]
    (normalize x data false))
  ([x data ^boolean merge-refs]
   (let [refs (atom {})
         x    (if (vector? x) x (get-query x))
         ret  (normalize* x data refs)]
     (if merge-refs
       (merge ret @refs)
       (with-meta ret @refs)))))

(defn- sift-refs [res]
  (let [{refs true rest false} (group-by #(vector? (first %)) res)]
    [(into {} refs) (into {} rest)]))

;; TODO: revisit

(defn ^boolean ref? [x]
  (and (vector? x)
       (== 2 (count x))
       (keyword? (nth x 0))))

;; TODO: easy to optimize

(defn denormalize
  "Given a selector, normalized data, and the normalized application state
   return the denormalized data."
  [selector data refs]
  (if (vector? data)
    (into [] (map #(denormalize selector (get-in refs %) refs)) data)
    (let [{props false joins true} (group-by join? selector)]
      (loop [joins (seq joins) ret {}]
        (if-not (nil? joins)
          (let [join      (first joins)
                [key sel] (join-value join)
                v         (get data key)]
            (if-not (ref? v)
              (recur (next joins)
                (assoc ret key (denormalize sel v refs)))
              (recur (next joins)
                (assoc ret key (denormalize sel (get-in refs v) refs)))))
          (merge (select-keys data props) ret))))))

;; =============================================================================
;; Reconciler

(defn- queue-calls! [reconciler res]
  (p/queue! reconciler (into [] (remove symbol?) (keys res))))

(defn- merge-refs [tree config refs]
  (let [{:keys [merge-ref indexer]} config]
    (letfn [(step [tree' [ref props]]
              (if (:normalize config)
                (let [c      (ref->any indexer ref)
                      props' (normalize c props)
                      refs   (meta props')]
                  ((:merge-tree config) (merge-ref config tree' ref props') refs))
                (merge-ref config tree' ref props)))]
      (reduce step tree refs))))

(defn- merge-novelty!
  [reconciler res]
  (let [config      (:config reconciler)
        root        (:root @(:state reconciler))
        [refs res'] (sift-refs res)
        res'        (if (:normalize config)
                      (normalize root res' true)
                      res')]
    (swap! (:state config)
      #(-> %
        (merge-refs config refs)
        ((:merge-tree config) res')))))

(defn merge!
  "Merge a state delta into the application state. Affected components managed
   by the reconciler will re-render."
  [reconciler delta]
  (queue-calls! reconciler delta)
  (merge-novelty! reconciler delta))

(defrecord Reconciler [config state]
  IDeref
  (-deref [this] @(:state config))

  p/IReconciler
  (basis-t [_] (:t @state))

  (add-root! [this root-class target options]
    (let [ret   (atom nil)
          rctor (factory root-class)]
      (when (satisfies? IQuery root-class)
        (p/index-root (:indexer config) root-class))
      (when (and (:normalize config)
                 (not (:normalized @state)))
        (let [new-state (normalize root-class @(:state config))
              refs      (meta new-state)]
          (reset! (:state config) (merge new-state refs))
          (swap! state assoc :normalized true)
          (p/queue! this [::skip])))
      (let [renderf (fn [data]
                      (binding [*reconciler* this
                                *shared*     (:shared config)]
                        (let [c ((:root-render config) (rctor data) target)]
                          (when (nil? @ret)
                            (swap! state assoc :root c)
                            (reset! ret c)))))
            parsef  (fn []
                      (let [sel (get-query (or @ret root-class))]
                        (if-not (nil? sel)
                          (let [env  (to-env config)
                                v    ((:parser config) env sel)
                                snds (gather-sends env sel (:remotes config))]
                            (when-not (empty? v)
                              (renderf v))
                            (when-not (empty? snds)
                              (when-let [send (:send config)]
                                (send snds
                                  #(do
                                    (merge-novelty! this %)
                                    (renderf ((:parser config) env sel)))))))
                          (renderf @(:state config)))))]
        (swap! state merge
          {:target target :render parsef :root root-class
           :remove (fn []
                     (remove-watch (:state config) target)
                     (swap! state
                       #(-> %
                         (dissoc :target) (dissoc :render) (dissoc :root)
                         (dissoc :remove)))
                     ((:root-unmount config) target))})
        (add-watch (:state config) target
          (fn [_ _ _ _]
            (swap! state update-in [:t] inc)
            (schedule-render! this)))
        (parsef)
        ret)))

  (remove-root! [_ target]
    (when-let [remove (:remove @state)]
      (remove)))

  (reindex! [_]
    (let [root (get @state :root)]
      (when (satisfies? IQuery root)
        (p/index-root (:indexer config) root))))

  (queue! [_ ks]
    (swap! state update-in [:queue] into ks))

  (queue-sends! [_ sends]
    (swap! state update-in [:queued-sends]
      (:merge-sends config) sends))

  (schedule-render! [_]
    (if-not (:queued @state)
      (swap! state update-in [:queued] not)
      false))

  (schedule-sends! [_]
    (if-not (:sends-queued @state)
      (do
        (swap! state assoc [:sends-queued] true)
        true)
      false))

  ;; TODO: need to reindex roots after reconcilation
  (reconcile! [_]
    (let [st @state
          q  (:queue st)]
      (cond
        (empty? q) ((:render st))

        (= [::skip] q) nil

        :else
        (let [cs (transduce
                   (map #(p/key->components (:indexer config) %))
                   #(into %1 %2) #{} q)
              {:keys [ui->props]} config
              env (to-env config)]
          (doseq [c ((:optimize config) cs)]
            (when (mounted? c)
              (let [computed   (get-computed (props c))
                    next-props (om.next/computed (ui->props env c) computed)]
                (when (should-update? c next-props (get-state c))
                  (if-not (nil? next-props)
                    (update-component! c next-props)
                    (.forceUpdate c))))))))
      (swap! state assoc :queue [])
      (swap! state update-in [:queued] not)))

  (send! [this]
    (let [sends (:queued-sends @state)]
      (when-not (empty? sends)
        (swap! state
          (fn [state]
            (-> state
              (assoc :queued-sends {})
              (assoc :sends-queued false))))
        ((:send config) sends
          #(do
             (queue-calls! this %)
             (merge-novelty! this %)))))))

(defn- default-ui->props
  [{:keys [parser] :as env} c]
  (let [path (path c)
        fq   (full-query c path)]
    (when-not (nil? fq)
      (let [s  (system-time)
            ui (parser env fq)
            e  (system-time)]
        (when-not (nil? *logger*)
          (let [dt (- e s)]
            (when (< 16 dt)
              (glog/warning *logger* (str c " query took " dt " msecs")))))
        (get-in ui path)))))

(defn- default-merge-ref
  [_ tree ref props]
  (update-in tree ref merge props))

(defn reconciler
  "Construct a reconciler from a configuration map, the following options
   are required:

   :state   - the application state, must be IAtom.
   :parser  - the parser to be used
   :send    - required only if the parser will return a non-empty value when
              run against the supplied :remotes. send is a function of two
              arguments, the map of remote expressions keyed by remote target
              and a callback which should be invoked with the result from each
              remote target. Note this means the callback can be invoked
              multiple times to support parallel fetching and incremental
              loading if desired.
   :remotes - a vector of keywords representing remote services which can
              evaluate query expressions. Defaults to [:remote]"
  [{:keys [state shared parser indexer
           ui->props normalize
           send merge-sends remotes
           merge-tree merge-ref
           optimize
           history
           root-render root-unmount]
    :or {ui->props    default-ui->props
         indexer      om.next/indexer
         merge-sends  #(merge-with into %1 %2)
         remotes      [:remote]
         merge-tree   #(merge-with merge %1 %2)
         merge-ref    default-merge-ref
         optimize     (fn [cs] (sort-by depth cs))
         history      100
         root-render  #(js/ReactDOM.render %1 %2)
         root-unmount #(js/ReactDOM.unmountComponentAtNode %)}
    :as config}]
  {:pre [(map? config)]}
  (let [idxr   (indexer)
        norm?  (satisfies? IAtom state)
        state' (if norm? state (atom state))
        ret    (Reconciler.
                 {:state state' :shared shared :parser parser :indexer idxr
                  :ui->props ui->props
                  :send send :merge-sends merge-sends :remotes remotes
                  :merge-tree merge-tree :merge-ref merge-ref
                  :optimize optimize
                  :normalize (or (not norm?) normalize)
                  :history (c/cache history)
                  :root-render root-render :root-unmount root-unmount}
                 (atom {:queue [] :queued false :queued-sends {}
                        :sends-queued false
                        :target nil :root nil :render nil :remove nil
                        :t 0 :normalized false}))]
    ret))

(defn ^boolean reconciler?
  "Returns true if x is a reconciler."
  [x]
  (instance? Reconciler x))

(defn app-state
  "Return the reconciler's application state atom. Useful when the reconciler
   was initialized via denormalized data."
  [reconciler]
  (-> reconciler :config :state))

(defn from-history
  "Given a reconciler and UUID return the application state snapshost
   from history associated with the UUID. The size of the reconciler history
   may be configured by the :history option when constructing the reconciler."
  [reconciler uuid]
  (.get (-> reconciler :config :history) uuid))
