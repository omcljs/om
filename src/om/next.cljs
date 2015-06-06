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
  (params [this]))

(extend-type default
  IQueryParams
  (params [_]))

(defprotocol IQuery
  (query [this]))

(defprotocol IAssert
  (handle-assert! [handler entity context]))

(defprotocol IRetract
  (handle-retract! [handler entity context]))

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

(defn get-query [cl]
  (with-meta (bind-query (query cl) (params cl)) {:component cl}))

;; =============================================================================
;; React Bridging

(defn create-factory [cl]
  (fn [props children]
    (if *instrument*
      (*instrument* props children)
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

(defn reconciler [c]
  (.. c -props -omcljs$reconciler))

(defn t [c]
  (.. c -props -omcljs$t))

(defn root-class [c]
  (.. c -props -omcljs$rootClass))

(defn parent [c]
  (.. c -props -omcljs$parent))

(defn depth [c]
  (.. c -props -omcljs$depth))

(defn react-key [c]
  (.. c -props -key))

(defn index [c]
  (.. c -props -omcljs$index))

(defn shared [c]
  (.. c -props -omcljs$shared))

(defn instrument [c]
  (.. c -props -omcljs$instrument))

(defn update-props! [c next-props]
  (set! (.. c -props -omcljs$t) (p/basis-t (reconciler c)))
  (set! (.. c -props -omcljs$value) next-props))

(defn props [c]
  (let [r (reconciler c)]
    (if (= (t c) (p/basis-t r))
      ;; fresh
      (.. c -props -omcljs$value)
      ;; stale
      (p/props-for r c))))

(defn set-state! [c new-state]
  (if (satisfies? ILocalState c)
    (-set-state! c new-state)
    (set! (.. c -state -omcljs$pendingState) new-state)))

(defn get-state [c]
  (if (satisfies? ILocalState c)
    (-get-state c)
    (when-let [state (. c -state)]
      (or (. state -omcljs$pendingState)
          (. state -omcljs$state)))))

(defn get-rendered-state [c]
  (if (satisfies? ILocalState c)
    (-get-rendered-state c)
    (some-> c .-state .-omcljs$state)))

(defn merge-pending-state! [c]
  (if (satisfies? ILocalState c)
    (-merge-pending-state! c)
    (when-let [pending (some-> c .-state .-omcljs$pendingState)]
      (let [previous (.. c -state -omcljs$state)]
        (gobj/remove (. c -state) "omcljs$pendingState")
        (set! (.. c -state -omcljs$previousState) previous)
        (set! (.. c -state -omcljs$state) pending)))))

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

(defn commit! [c tx-data]
  (let [r (reconciler c)]
    (p/commit! r tx-data c)
    (schedule! r)))

(defn assert! [origin entity]
  (loop [c origin]
    (cond
      (satisfies? IAssert c) (handle-assert! c entity origin)
      (nil? c) (commit! origin entity)
      :else (recur (parent c)))))

(defn retract! [origin entity]
  (loop [c origin]
    (cond
      (satisfies? IRetract c) (handle-retract! c entity origin)
      (nil? c) (throw
                 (ex-info
                   (str "No retraction handler found for component of type "
                     (type c))
                   {:type ::missing-retract-handler}))
      :else (recur (parent c)))))

;; =============================================================================
;; Default Reconciler

(defn build-index [cl]
  (let [component->path (atom {})
        prop->component (atom {})
        rootq (get-query cl)]
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
         {} @component->path)})))

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
                 (commit! [_ next-props component]
                   (let [index (index component)
                         path  (cond->
                                 (get-in @idxs
                                   [(root-class component)
                                    :component->path (type component)])
                                 index (conj index))]
                     (swap! t inc) ;; TODO: probably should revisit doing this here
                     (swap! queue conj [component next-props])
                     (swap! state p/push next-props path)))
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
                           sel     (get-query root-class)
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
