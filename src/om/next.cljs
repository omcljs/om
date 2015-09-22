(ns om.next
  (:refer-clojure :exclude [var? key])
  (:require-macros [om.next :refer [defui]])
  (:require [goog.string :as gstring]
            [goog.object :as gobj]
            [goog.dom :as gdom]
            [clojure.walk :as walk]
            [om.next.protocols :as p]
            [om.next.impl.parser :as parser]))

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

(defn ^boolean nil-or-map? [x]
  (or (nil? x) (map? x)))

(defn focus-query [sel path]
  (if (empty? path)
    sel
    (let [[k & ks] path]
     (letfn [(match [x]
               (let [k' (if (map? x) (ffirst x) x)]
                 (= k k')))
             (value [x]
               (if (map? x)
                 {(ffirst x) (focus-query (-> x first second) ks)}
                 x))]
       (into [] (comp (filter match) (map value)) sel)))))

(defn focus->path
  ([focus] (focus->path focus []))
  ([focus path]
   {:pre [(vector? focus)]}
   (if (and (some map? focus)
            (== 1 (count focus)))
     (let [[k focus'] (ffirst focus)]
       (recur focus' (conj path k)))
     path)))

;; =============================================================================
;; Query Protocols & Helpers

(defprotocol IQueryParams
  (params [this]))

(extend-type default
  IQueryParams
  (params [_]))

(defprotocol IQuery
  (query [this]))

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
  (with-meta (bind-query (query cl) (params cl))
    {:component cl}))

(defn iquery? [x]
  (satisfies? IQuery x))

;; =============================================================================
;; React Bridging

(defn- compute-react-key [cl props]
  (if-let [rk (:react-key props)]
    rk
    (if-let [idx (:om-index props)]
      (str (. cl -name) "_" idx)
      js/undefined)))

(defn create-factory [cl]
  {:pre [(fn? cl)]}
  (fn [props & children]
    (if *instrument*
      (apply *instrument* props children)
      (js/React.createElement cl
        #js {:key (compute-react-key cl props)
             :omcljs$value props
             :omcljs$index (:om-index props)
             :omcljs$reconciler *reconciler*
             :omcljs$rootClass *root-class*
             :omcljs$parent *parent*
             :omcljs$shared *shared*
             :omcljs$instrument *instrument*
             :omcljs$depth *depth*
             :omcljs$t (when *reconciler* (p/basis-t *reconciler*))}
        children))))

(defn ^boolean component? [x]
  (. x -om$isComponent))

(defn state [c]
  {:pre [(component? c)]}
  (.-state c))

(defn- get-prop [c k]
  (gobj/get (.-props c) k))

(defn- set-prop! [c k v]
  (gobj/set (.-props c) k v))

(defn get-reconciler [c]
  {:pre [(component? c)]}
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

(defn react-type [x]
  (or (gobj/get x "type") (type x)))

(defn index [c]
  (get-prop c "omcljs$index"))

(defn shared [c]
  {:pre [(component? c)]}
  (get-prop c "omcljs$shared"))

(defn instrument [c]
  {:pre [(component? c)]}
  (get-prop c "omcljs$instrument"))

(defn update-props! [c next-props]
  {:pre [(component? c)]}
  (gobj/set (.-state c) "omcljs$t" (p/basis-t (get-reconciler c)))
  (gobj/set (.-state c) "omcljs$value" next-props))

(defn props [c]
  {:pre [(component? c)]}
  (let [cst (.-state c)
        cps (.-props c)]
    (if (nil? cst)
      (gobj/get cps "omcljs$value")
      (let [t0 (gobj/get cst "omcljs$t")
            t1 (gobj/get cps "omcljs$t")]
        (if (> t0 t1)
          (gobj/get cst "omcljs$value")
          (gobj/get cps "omcljs$value"))))))

(defn set-state! [c new-state]
  {:pre [(component? c)]}
  (if (satisfies? ILocalState c)
    (-set-state! c new-state)
    (gobj/set (.-state c) "omcljs$pendingState" new-state)))

(defn get-state
  ([c]
   (get-state c []))
  ([c & ks]
   {:pre [(component? c)]}
   (let [cst (if (satisfies? ILocalState c)
               (-get-state c)
               (when-let [state (. c -state)]
                 (or (gobj/get state "omcljs$pendingState")
                   (gobj/get state "omcljs$state"))))]
     (get-in cst ks))))

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
  {:pre [(component? c)]}
  (if (satisfies? ILocalState c)
    (-get-rendered-state c)
    (some-> c .-state (gobj/get "omcljs$state"))))

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
   {:pre [(component? c)]}
   (.setState c #js {:omcljs$state new-state} nil)))

;; TODO: where to put query mutations so that time travel can be preserved?
;; TODO: will need to reindex

(defn update-query! [c bs]
  )

(defn mounted? [c]
  {:pre [(component? c)]}
  (.isMounted c))

(defn dom-node
  ([c]
   (.getDOMNode c))
  ([c name]
   (some-> (.-refs c) (gobj/get name) (.getDOMNode))))

(defn react-ref
  [c name]
  (some-> (.-refs c) (gobj/get name)))

(defn children
  [c]
  (.. c -props -children))

(defn update-component! [c next-props]
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

(defn class-path [c]
  {:pre [(component? c)]}
  (loop [c c ret (list (type c))]
    (if-let [p (parent c)]
      (recur p (cons (type p) ret))
      ret)))

(defn data-path
  ([c]
   (let [f (fn [c] (and (iquery? c) (index c)))]
     (data-path c f)))
  ([c f]
   {:pre [(component? c) (fn? f)]}
   (loop [c c ret (list (or (f c) '*))]
     (if-let [p (parent c)]
       (recur p (cons (or (f p) '*) ret))
       ret))))

(defn focused? [x]
  (and (vector? x)
       (== 1 (count x))
       (map? (first x))))

(defn state-query [focus data-path]
  (letfn [(state-query* [focus data-path]
            (if (focused? focus)
              (let [[k v] (ffirst focus)
                    index (first data-path)]
                (if-not (= '* index)
                  [(list {k (state-query* v (rest data-path))} {:index index})]
                  [{k (state-query* v (rest data-path))}]))
              focus))]
    (state-query* focus (rest data-path))))

(defn state-path [focus data-path]
  (loop [focus focus data-path (rest data-path) ret []]
    (if (focused? focus)
      (let [[k v] (ffirst focus)
            index (first data-path)]
        (recur v (rest data-path)
          (cond-> (conj ret k)
            (not= '* index) (conj index))))
      ret)))

;; =============================================================================
;; Reconciler API

(declare reconciler?)

(defn basis-t [reconciler]
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

(defn schedule-send! [reconciler]
  (when (p/schedule-send! reconciler)
    (js/setTimeout #(p/send! reconciler) 500)))

(defn add-root!
  ([reconciler target root-class]
   (add-root! reconciler target root-class nil))
  ([reconciler target root-class options]
   {:pre [(reconciler? reconciler) (gdom/isElement target) (fn? root-class)]}
   (p/add-root! reconciler target root-class options)))

(defn remove-root! [reconciler target]
  (p/remove-root! reconciler target))

;; =============================================================================
;; Refs

(deftype Ref [path]
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer (str "#object[om.next.Ref " path "]")))
  IHash
  (-hash [this] (-hash path))
  IEquiv
  (-equiv [this other]
    (and (instance? Ref other)
         (= path (.-path other))))
  ISeqable
  (-seq [this] (seq path))
  ILookup
  (-lookup [this k] (-lookup this k nil))
  (-lookup [this k not-found]
    (case k
      :root (nth path 0)
      :id   (nth path 1)
      not-found))
  IIndexed
  (-nth [this i]
    (-nth this i))
  (-nth [this i not-found]
    (case i
      0 (nth path 0)
      1 (nth path 1)
      not-found))
  ICollection
  (-conj [this x] (Ref. (conj path x)))
  IStack
  (-peek [this] (peek path))
  (-pop [this] (Ref. (pop path))))

(defn ref [root id & more]
  (Ref. (into [root id] more)))

(defn ^boolean ref? [x]
  (instance? Ref x))

(defn refs [root & ids]
  (into [] (map #(ref root %)) ids))

;; =============================================================================
;; Call Support

(defn call
  ([c name] (call c name nil))
  ([c name param-map]
   {:pre [(component? c) (symbol? name) (nil-or-map? param-map)]}
   (let [r   (get-reconciler c)
         cfg (:config r)
         ref ((:ui->ref cfg) c)
         env (merge
               (select-keys cfg [:indexer :parser :state])
               {:reconciler r :component c}
               (when ref
                 {:ref ref}))
         exp `[(~name ~param-map)]
         v   ((:parser cfg) env exp)
         v'  ((:parser cfg) env exp true)]
     (when-not (empty? v)
       (p/queue! r (reduce into (if ref [ref] []) (vals v))))
     (when-not (empty? v')
       (p/queue-send! r v')
       (schedule-send! r)))))

;; =============================================================================
;; Parser

(defn parser [opts]
  {:pre [(map? opts)]}
  (parser/parser opts))

;; =============================================================================
;; Indexer

(defrecord Indexer [indexes ui->ref]
  p/IIndexer

  (index-root [_ klass]
    (let [class->paths     (atom {})
          prop->classes    (atom {})
          class-path->query (atom {})
          rootq            (get-query klass)]
      (letfn [(build-index* [klass selector path classpath]
                (swap! class->paths update-in [klass]
                  (fnil conj #{}) path)
                (swap! class-path->query assoc classpath
                  (focus-query rootq path))
                (let [{props true joins false} (group-by keyword? selector)]
                  (swap! prop->classes
                    #(merge-with into % (zipmap props (repeat #{klass}))))
                  (doseq [join joins]
                    (let [[prop selector'] (first join)]
                      (swap! prop->classes
                        #(merge-with into % {prop #{klass}}))
                      (let [klass' (-> selector' meta :component)]
                        (build-index* klass' selector'
                          (conj path prop) (conj classpath klass')))))))]
        (build-index* klass rootq [] [klass])
        (reset! indexes
          {:prop->classes @prop->classes
           :class->paths @class->paths
           :class->selectors
           (reduce-kv
             (fn [ret class paths]
               (assoc ret class (into #{} (map #(focus-query rootq %)) paths)))
             {} @class->paths)
           :class->components {}
           :ref->components {}
           :class-path->query @class-path->query
           :component->path {}}))))

  (index-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [ref (ui->ref c)]
          (cond-> (update-in indexes [:class->components (type c)] (fnil conj #{}) c)
            ref (update-in [:ref->components ref] (fnil conj #{}) c))))))

  (drop-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [ref (ui->ref c)]
          (cond-> (update-in indexes [:class->components (type c)] disj c)
            ref (update-in [:ref->components ref] disj c))))))

  (ref-for [_ component]
    (ui->ref component))

  (key->components [_ k]
    (let [indexes @indexes]
      (cond
        (component? k) #{k}
        (ref? k) (get-in indexes [:ref->components k])
        (keyword? k) (let [cs (get-in indexes [:prop->classes k])]
                       (transduce (map #(get-in indexes [:class->components %]))
                         (completing into) #{} cs))
        :else (throw (js/Error. (str "Invalid key " k ", key must be ref or keyword")))))))

(defn indexer [ui->ref]
  (Indexer. (atom {}) ui->ref))

(defn ^boolean indexer? [x]
  (instance? Indexer x))

(defn build-index
  ([class] (build-index class identity))
  ([class ui->ref]
    (let [idxr (indexer ui->ref)]
      (p/index-root idxr class))))

;; =============================================================================
;; Reconciler

(defn queue-calls! [r res]
  (let [call-ks (into [] (filter symbol?) (keys res))]
    (p/queue! r (transduce (map res) (completing into) [] call-ks))))

(defrecord Reconciler [config state]
  p/IReconciler

  (basis-t [_] (:t @state))

  (add-root! [this target root-class options]
    (let [ret (atom nil)
          rctor (create-factory root-class)]
      (p/index-root (:indexer config) root-class)
      (let [renderf (fn [data]
                      (binding [*reconciler* this
                                *root-class* root-class]
                        (reset! ret
                          (js/React.render (rctor data) target))))
            sel     (get-query root-class)]
        (swap! state update-in [:roots] assoc target renderf)
        (let [env (assoc (select-keys config [:state :indexer :parser])
                    :reconciler this)
              v   ((:parser config) env sel)
              v'  ((:parser config) env sel true)]
          (when-not (empty? v)
            (renderf v))
          (when-not (empty? v')
            (when-let [send (:send config)]
              (send v'
                (fn [res]
                  (queue-calls! this res)
                  (swap! (:state config) (:merge-state config) res))))))
        @ret)))

  (remove-root! [_ target]
    (swap! state update-in [:roots] dissoc target))

  (queue! [_ k-or-ks]
    (swap! state
      (fn [state]
        (-> state
          (update-in [:t] inc) ;; TODO: probably should revisit doing this here
          (update-in [:queue]
            (fn [queue]
              (let [ks (if-not (sequential? k-or-ks) [k-or-ks] k-or-ks)]
                (into queue ks))))))))

  (queue-send! [_ expr]
    (swap! state update-in [:queued-send]
      (:merge-send config) expr))

  (schedule-render! [_]
    (if-not (:queued @state)
      (swap! state update-in [:queued] not)
      false))

  (schedule-send! [_]
    (if-not (:send-queued @state)
      (swap! state update-in [:send-queued] not)
      false))

  ;; TODO: need to reindex roots after reconcilation
  (reconcile! [_]
    (let [st @state]
      (if (empty? (:queue st))
        (doseq [[_ renderf] (:roots st)]
          (renderf @(:state config)))
        (let [cs (transduce (map #(p/key->components (:indexer config) %))
                   (completing into) #{} (:queue st))
              {:keys [ui->props state]} config
              env (select-keys config [:state :parser :indexer :ui->ref])]
          (doseq [c ((:optimize config) cs)]
            (let [next-props (ui->props env c)]
              (when (and (should-update? c next-props)
                         (mounted? c))
                (update-component! c next-props))))
          (swap! state assoc :queue [])))
      (swap! state update-in [:queued] not)))

  (send! [this]
    (let [expr (:pending-send @state)]
      (when expr
        (swap! state assoc :pending-state nil)
        ((:send config) expr
          (fn [res]
            (queue-calls! this res)
            (swap! (:state config) (:merge-state config) res)))))))

(defn default-ui->props
  [{:keys [state indexer parser] :as env} c]
  (let [st   @state
        idxs @(:indexes indexer)
        fcs  (get-in idxs [:class-path->query (class-path c)])
        ps   (get-in (parser env fcs) (state-path fcs (data-path c)))]
    (if (ref? ps)
      (let [{:keys [root id]} ps]
        (get-in st [root id]))
      ps)))

(defn reconciler
  [{:keys [state parser indexer resolve
           ui->ref ui->props
           send merge-send merge-state
           optimize]
    :or {ui->ref     identity
         ui->props   default-ui->props
         indexer     om.next/indexer
         merge-send  into
         merge-state merge
         optimize    (fn [cs] (sort-by depth cs))}
    :as config}]
  {:pre [(map? config)]}
  (let [idxr (indexer ui->ref)
        ret  (Reconciler.
               {:state state :parser parser :indexer idxr :resolve resolve
                :ui->ref ui->ref :ui->props ui->props
                :send send :merge-send merge-send :merge-state merge-state
                :optimize optimize}
               (atom {:queue [] :queued false :pending-send nil
                      :send-queued false :roots {} :t 0}))]
    (add-watch state :om/reconciler
      (fn [_ _ _ _] (schedule-render! ret)))
    ret))

(defn ^boolean reconciler? [x]
  (instance? Reconciler x))