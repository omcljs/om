(ns om.next
  (:refer-clojure :exclude [var? key])
  (:require-macros [om.next :refer [defui]])
  (:require [goog.string :as gstring]
            [goog.object :as gobj]
            [clojure.walk :as walk]
            [om.next.protocols :as p]
            [om.next.stores :refer [TreeStore]]))

;; =============================================================================
;; Globals & Dynamics

(def ^{:dynamic true} *raf* nil)

(def ^{:dynamic true :private true} *reconciler* nil)

(def ^{:dynamic true :private true} *root-class* nil)

(def ^{:dynamic true :private true} *parent* nil)

(def ^{:dynamic true :private true} *shared* nil)

(def ^{:dynamic true :private true} *instrument* nil)

(def ^{:dynamic true :private true} *depth* 0)

;; =============================================================================
;; Utilities

(defn filter-selector [sel path]
  (if (empty? path)
    sel
    (let [[k & ks] path]
     (letfn [(match [x]
               (let [k' (if (map? x) (ffirst x) x)]
                 (= k k')))
             (value [x]
               (if (map? x)
                 {(ffirst x) (filter-selector (-> x first second) ks)}
                 x))]
       (into [] (comp (filter match) (map value)) sel)))))

;; =============================================================================
;; Query Protocols & Helpers

(defprotocol IQueryParams
  (-params [this]))

(extend-type default
  IQueryParams
  (-params [_]))

(defprotocol IQuery
  (-query [this]))

(defprotocol IAssert
  (-assert [handler entity context]))

(defprotocol IRetract
  (-retract [handler entity context]))

(defprotocol ILocalState
  (-set-state! [this new-state])
  (-get-state [this])
  (-get-rendered-state [this])
  (-merge-pending-state! [this]))

(defn var? [x]
  (and (symbol? x)
       (gstring/startsWith (name x) "?")))

(defn var->keyword [x]
  (keyword (.substring (name x) 1)))

(defn bind-query [query params]
  (letfn [(replace-var [node]
            (if (var? node)
              (get params (var->keyword node) node)
              node))]
    (walk/prewalk replace-var query)))

(defn query [cl]
  (with-meta (bind-query (-query cl) (-params cl))
    {:component cl}))

;; =============================================================================
;; React Bridging

(defn create-factory [cl]
  (fn [props & children]
    (if *instrument*
      (apply *instrument* props children)
      (let [m (meta props)]
        (js/React.createElement cl
          #js {:key (:react-key m)
               :omcljs$value props
               :omcljs$index (::index m)
               :omcljs$reconciler *reconciler*
               :omcljs$rootClass *root-class*
               :omcljs$parent *parent*
               :omcljs$shared *shared*
               :omcljs$instrument *instrument*
               :omcljs$depth *depth*
               :omcljs$t (when *reconciler* (p/basis-t *reconciler*))}
          children)))))

(defn state [c]
  (.-state c))

(defn- get-prop [c k]
  (gobj/get (.-props c) k))

(defn- set-prop! [c k v]
  (gobj/set (.-props c) k v))

(defn reconciler [c]
  (get-prop c "omcljs$reconciler"))

(defn t [c]
  (get-prop c "omcljs$t"))

(defn root-class [c]
  (get-prop c "omcljs$rootClass"))

(defn parent [c]
  (get-prop c "omcljs$parent"))

(defn depth [c]
  (get-prop c "omcljs$depth"))

(defn react-key [c]
  (.. c -props -key))

(defn index [c]
  (get-prop c "omcljs$index"))

(defn shared [c]
  (get-prop c "omcljs$shared"))

(defn instrument [c]
  (get-prop c "omcljs$instrument"))

(defn update-props! [c next-props]
  (set-prop! c "omcljs$t" (p/basis-t (reconciler c)))
  (set-prop! c "omcljs$value" next-props))

(defn props [c]
  (let [r (reconciler c)]
    (if (or (nil? r)
            (= (t c) (p/basis-t r)))
      ;; fresh
      (get-prop c "omcljs$value")
      ;; stale
      (p/props-for r c))))

(defn set-state! [c new-state]
  (if (satisfies? ILocalState c)
    (-set-state! c new-state)
    (gobj/set (.-state c) "omcljs$pendingState" new-state)))

(defn get-state [c]
  (if (satisfies? ILocalState c)
    (-get-state c)
    (when-let [state (. c -state)]
      (or (gobj/get state "omcljs$pendingState")
          (gobj/get state "omcljs$state")))))

(defn update-state!
  ([c f]
   (set-state! c (f (get-state c))))
  ([c f arg0]
   (set-state! c (f (get-state c) arg0)))
  ([c f arg0 arg1]
   (set-state! c (f (get-state c) arg0 arg1)))
  ([c f arg0 arg1 arg2]
   (set-state! c (f (get-state c) arg0 arg1 arg2)))
  ([c f arg0 arg1 arg2 arg3]
   (set-state! c (f (get-state c) arg0 arg1 arg2 arg3)))
  ([c f arg0 arg1 arg2 arg3 & arg-rest]
   (set-state! c
     (apply f (get-state c) arg0 arg1 arg2 arg3 arg-rest))))

(defn get-rendered-state [c]
  (if (satisfies? ILocalState c)
    (-get-rendered-state c)
    (some-> c .-state .-omcljs$state)))

(defn merge-pending-state! [c]
  (if (satisfies? ILocalState c)
    (-merge-pending-state! c)
    (when-let [pending (some-> c .-state (gobj/get "omcljs$pendingState"))]
      (let [state    (.-state c)
            previous (gobj/get state "omcljs$state")]
        (gobj/remove state "omcljs$pendingState")
        (gobj/set state "omcljs$previousState" previous)
        (gobj/set state "omcljs$state" pending)))))

(defn react-set-state!
  ([c new-state]
   (react-set-state! c new-state nil))
  ([c new-state cb]
   (.setState c #js {:omcljs$state new-state} nil)))

(defn update-query! [c bs])

(defn mounted? [c]
  (.isMounted c))

(defn dom-node
  ([c]
   (.getDOMNode c))
  ([c name]
   (some-> (.-refs c) (gobj/get name) (.getDOMNode))))

(defn ref
  [c name]
  (some-> (.-refs c) (gobj/get name)))

(defn children
  [c]
  (.. c -props -children))

(defn update-component! [c next-props]
  (update-props! c next-props)
  (.forceUpdate c))

(defn should-update?
  ([c next-props]
   (should-update? c next-props nil))
  ([c next-props next-state]
   (.shouldComponentUpdate c
     #js {:omcljs$value next-props}
     #js {:omcljs$state next-state})))

;; FIXME: this needs conceptual work
(defn map-keys
  ([ctor xs] (map-keys ctor nil xs))
  ([ctor keyfn xs]
    (cond
      (nil? keyfn)
      (map-indexed #(ctor (with-meta %2 {:react-key %1 ::index %1})) xs)

      (or (keyword? keyfn) (fn? keyfn))
      (map-indexed #(ctor (with-meta %2 {:react-key (keyfn %2) ::index %1})) xs)

      :else
      (throw (ex-info (str "Invalid keyfn " keyfn)
               {:type ::invalid-keyfn})))))

;; =============================================================================
;; Reconciler API

(defn store [reconciler]
  (p/store reconciler))

(defn indexes [reconciler]
  (p/indexes reconciler))

(defn basis-t [reconciler]
  (p/basis-t reconciler))

(defn schedule! [reconciler]
  (when (p/schedule! reconciler)
    (let [f #(p/reconcile! reconciler)]
      (cond
        (fn? *raf*) (*raf* f)

        (not (exists? js/requestAnimationFrame))
        (js/setTimeout f 16)

        :else
        (js/requestAnimationFrame f)))))

(defn add-root!
  ([reconciler target root-class]
   (add-root! reconciler target root-class nil))
  ([reconciler target root-class options]
   (p/add-root! reconciler target root-class options)))

(defn remove-root! [reconciler target]
  (p/remove-root! reconciler target))

;; =============================================================================
;; State Transition

(defn transact! [tx-type c tx-data]
  (let [r (reconciler c)]
    (p/commit! r tx-type tx-data c)
    (schedule! r)))

(defn assert! [origin entity]
  (loop [c origin entity entity]
    (cond
      (satisfies? IAssert c)
      (when-let [entity (-assert c entity origin)]
        (recur (parent c) entity))

      (nil? c)
      (transact! :assert origin entity)

      :else (recur (parent c) entity))))

(defn retract! [origin entity]
  (loop [c origin entity entity]
    (cond
      (satisfies? IRetract c)
      (when-let [entity (-retract c entity origin)]
        (recur (parent c) entity))

      (nil? c)
      (transact! :retract origin entity)

      :else (recur (parent c) entity))))

;; =============================================================================
;; Default Reconciler

(defn build-index [cl]
  (let [component->path (atom {})
        prop->component (atom {})
        rootq (query cl)]
    (letfn [(build-index* [cl sel path]
              (swap! component->path assoc cl path)
              (let [{ks true ms false} (group-by keyword? sel)]
                (swap! prop->component #(merge-with into % (zipmap ks (repeat #{cl}))))
                (doseq [m ms]
                  (let [[attr sel] (first m)]
                    (swap! prop->component #(merge-with into % {attr #{cl}}))
                    (let [cl (-> sel meta :component)]
                      (build-index* cl sel (conj path attr)))))))]
      (build-index* cl rootq [])
      {:prop->component @prop->component
       :component->path @component->path
       :component->selector
       (reduce-kv
         (fn [ret class path]
           (assoc ret class (filter-selector rootq path)))
         {} @component->path)
       :type->components {}})))

(defn tree-reconciler [data]
  (let [state  (cond
                 (satisfies? IAtom data) data
                 (satisfies? p/IStore data) (atom data)
                 (map? data) (atom (TreeStore. data))
                 :else (throw (ex-info "data must be an atom, store, or map"
                                {:type ::invalid-argument})))
        idxs   (atom {})
        queue  (atom [])
        queued (atom false)
        roots  (atom {})
        t      (atom 0)
        r      (reify
                 p/ICommitQueue
                 (commit! [_ tx-type next-props component]
                   (let [index (index component)
                         path  (cond->
                                 (get-in @idxs
                                   [(root-class component)
                                    :component->path (type component)])
                                 index (conj index))]
                     (swap! t inc) ;; TODO: probably should revisit doing this here
                     (swap! queue conj [component next-props])
                     (swap! state p/push next-props path)))
                 p/IComponentIndex
                 (index-component! [_ c]
                   (swap! idxs update-in [:type->components (type c)] (fnil conj #{}) c))
                 (drop-component! [_ c]
                   (swap! idxs update-in [:type->components (type c)] disj c))
                 p/IReconciler
                 (basis-t [_] @t)
                 (store [_] @state)
                 (indexes [_] @idxs)
                 (props-for [_ component]
                   (let [rc    (root-class component)
                         ct    (type component)
                         index (index component)
                         state @state
                         path  (cond->
                                 (get-in @idxs [rc :component->path ct])
                                 index (conj index))]
                     (get-in
                       (p/pull state
                         (get-in @idxs [rc :component->selector ct])
                         nil)
                       path)))
                 (add-root! [this target root-class options]
                   (let [ret (atom nil)
                         rctor (create-factory root-class)]
                     (swap! idxs assoc root-class (build-index root-class))
                     (let [renderf (fn [data]
                                     (binding [*reconciler* this
                                               *root-class* root-class]
                                       (reset! ret
                                         (js/React.render (rctor data) target))))
                           sel     (query root-class)
                           store   @state]
                       (swap! roots assoc target renderf)
                       (cond
                         (satisfies? p/IPullAsync store) (p/pull-async store sel nil renderf)
                         :else (renderf (p/pull store sel nil)))
                       @ret)))
                 (remove-root! [_ target]
                   (swap! roots dissoc target))
                 (schedule! [_]
                   (if-not @queued
                     (swap! queued not)
                     false))
                 (reconcile! [_]
                   (if (empty? @queue)
                     (do
                       (doseq [[_ renderf] @roots]
                         (renderf)))
                     (do
                       (doseq [[component next-props]
                               (sort-by (comp depth first) @queue)]
                         (when (should-update? component next-props)
                           (update-component! component next-props)))
                       (reset! queue [])))
                   (swap! queued not)))]
    (add-watch state :om/simple-reconciler
      (fn [_ _ _ _] (schedule! r)))
    r))
