(ns om.next
  (:refer-clojure :exclude [var? key])
  (:require-macros [om.next :refer [defui]])
  (:require [goog.string :as gstring]
            [clojure.walk :as walk]
            [om.next.protocols :as p]
            [om.next.stores :refer [TreeStore]]))

;; =============================================================================
;; Globals & Dynamics

(def ^{:dynamic true} *raf* nil)

(def ^{:dynamic true :private true} *reconciler* nil)

(def ^{:dynamic true :private true} *root-class* nil)

(def ^{:dynamic true :private true} *depth* 0)

;; =============================================================================
;; Query Protocols & Helpers

(defprotocol IQueryParams
  (params [this]))

(extend-type default
  IQueryParams
  (params [_]))

(defprotocol IQuery
  (query [this]))

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

(defn bound-query [cl]
  (with-meta (bind-query (query cl) (params cl)) {:component cl}))

;; =============================================================================
;; React Bridging

(defn create-factory [cl]
  (fn [props children]
    (js/React.createElement cl
      #js {:key (:react-key props)
           :omcljs$value props
           :omcljs$reconciler *reconciler*
           :omcljs$rootClass *root-class*
           :omcljs$depth *depth*}
      children)))

(defn props [c]
  (.. c -props -omcljs$value))

(defn update-props! [c next-props]
  (set! (.. c -props -omcljs$value) next-props))

(defn update-component! [c next-props]
  (update-props! c next-props)
  (.forceUpdate c))

(defn state [c]
  (.-state c))

(defn reconciler [c]
  (.. c -props -omcljs$reconciler))

(defn root-class [c]
  (.. c -props -omcljs$rootClass))

(defn depth [c]
  (.. c -props -omcljs$depth))

(defn react-key [c]
  (.. c -props -key))

(defn should-update? [c next-props]
  (.shouldComponentUpdate c #js {:omcljs$value next-props} (state c)))

;; =============================================================================
;; Reconciliation Fns

(defn schedule! [reconciler]
  (when (p/schedule! reconciler)
    (let [f #(p/reconcile! reconciler)]
      (cond
        (fn? *raf*) (*raf* f)

        (not (exists? js/requestAnimationFrame))
        (js/setTimeout f 16)

        :else
        (js/requestAnimationFrame f)))))

(defn commit! [c tx-data]
  (let [r (reconciler c)]
    (p/commit! r tx-data c)
    (schedule! r)))

;; =============================================================================
;; API

(defn add-root!
  ([reconciler target root-class]
   (add-root! reconciler target root-class nil))
  ([reconciler target root-class options]
   (p/add-root! reconciler target root-class options)))

(defn remove-root! [reconciler target]
  (p/remove-root! reconciler target))

;; =============================================================================
;; Default Reconciler

(defn build-index [cl]
  (let [component->path (atom {})
        prop->component (atom {})]
    (letfn [(build-index* [cl sel path]
              (swap! component->path assoc cl path)
              (let [{ks true ms false} (group-by keyword? sel)]
                (swap! prop->component #(merge-with into % (zipmap ks (repeat #{cl}))))
                (doseq [m ms]
                  (let [[attr sel] (first m)]
                    (swap! prop->component #(merge-with into % {attr #{cl}}))
                    (let [cl (-> sel meta :component)]
                      (build-index* cl sel (conj path attr)))))))]
      (build-index* cl (bound-query cl) [])
      {:prop->component @prop->component
       :component->path @component->path})))

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
        r      (reify
                 p/ICommitQueue
                 (commit! [_ next-props component]
                   (let [key (react-key component)
                         path (cond->
                                (get-in idxs
                                  [(root-class component)
                                   :component->path (type component)])
                                key (conj key))]
                     (swap! queue conj [component next-props])
                     (swap! state p/push next-props path)))
                 p/IReconciler
                 (add-root! [this target root-class options]
                   (let [ret (atom nil)
                         rctor (create-factory root-class)]
                     (swap! idxs assoc root-class (build-index root-class))
                     (let [renderf (fn [data]
                                     (binding [*reconciler* this
                                               *root-class* root-class]
                                       (reset! ret
                                         (js/React.render (rctor data) target))))
                           sel     (bound-query root-class)
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
