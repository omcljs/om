(ns om.next
  (:refer-clojure :exclude [deftype])
  (:require #?(:clj [cljs.core :refer [deftype specify! this-as js-arguments]])
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [clojure.string :as str]))

(defn collect-statics [dt]
  (letfn [(split-on-static [forms]
            (split-with (complement '#{static}) forms))
          (split-on-symbol [forms]
            (split-with (complement symbol?) forms))]
    (loop [dt (seq dt) dt' [] statics {:fields {} :protocols []}]
      (if dt
        (let [[pre [_ sym & remaining :as post]] (split-on-static dt)
              dt' (into dt' pre)]
          (if (seq post)
            (cond
              (= sym 'field)
              (let [[field-info dt] (split-at 2 remaining)]
                (recur (seq dt) dt'
                  (update-in statics [:fields] conj (vec field-info))))
              (symbol? sym)
              (let [[protocol-info dt] (split-on-symbol remaining)]
                (recur (seq dt) dt'
                  (update-in statics [:protocols]
                    into (concat [sym] protocol-info))))
              :else (throw #?(:clj  (IllegalArgumentException. "Malformed static")
                              :cljs (js/Error. "Malformed static"))))
            (recur nil dt' statics)))
        {:dt dt' :statics statics}))))

(defn- validate-statics [dt]
  (when-let [invalid (some #{"Ident" "IQuery" "IQueryParams"}
                       (map #(-> % str (str/split #"/") last)
                         (filter symbol? dt)))]
     (throw
       #?(:clj  (IllegalArgumentException.
                  (str invalid " protocol declaration must appear with `static`."))
          :cljs (js/Error.
                  (str invalid " protocol declaration must appear with `static`."))))))

(def lifecycle-sigs
  '{initLocalState [this]
    shouldComponentUpdate [this next-props next-state]
    componentWillReceiveProps [this next-props]
    componentWillUpdate [this next-props next-state]
    componentDidUpdate [this prev-props prev-state]
    componentWillMount [this]
    componentDidMount [this]
    componentWillUnmount [this]
    render [this]})

(defn validate-sig [[name sig :as method]]
  (let [sig' (get lifecycle-sigs name)]
    (assert (= (count sig') (count sig))
      (str "Invalid signature for " name " got " sig ", need " sig'))))

(def reshape-map
  {:reshape
   {'initLocalState
    (fn [[name [this :as args] & body]]
      `(~name ~args
         (let [ret# (do ~@body)]
           (cljs.core/js-obj "omcljs$state" ret#))))
    'componentWillReceiveProps
    (fn [[name [this next-props :as args] & body]]
      `(~name [this# next-props#]
         (let [~this this#
               ~next-props (om.next/-next-props next-props# this#)]
           ~@body)))
    'componentWillUpdate
    (fn [[name [this next-props next-state :as args] & body]]
      `(~name [this# next-props# next-state#]
         (let [~this       this#
               ~next-props (om.next/-next-props next-props# this#)
               ~next-state (or (goog.object/get next-state# "omcljs$pendingState")
                               (goog.object/get next-state# "omcljs$state"))
               ret#        (do ~@body)]
           (when (cljs.core/implements? om.next/Ident this#)
             (let [ident# (om.next/ident this# (om.next/props this#))
                   next-ident# (om.next/ident this# ~next-props)]
               (when (not= ident# next-ident#)
                 (let [idxr# (get-in (om.next/get-reconciler this#) [:config :indexer])]
                   (when-not (nil? idxr#)
                     (swap! (:indexes idxr#)
                       (fn [indexes#]
                         (-> indexes#
                           (update-in [:ref->components ident#] disj this#)
                           (update-in [:ref->components next-ident#] (fnil conj #{}) this#)))))))))
           (om.next/merge-pending-props! this#)
           (om.next/merge-pending-state! this#)
           ret#)))
    'componentDidUpdate
    (fn [[name [this prev-props prev-state :as args] & body]]
      `(~name [this# prev-props# prev-state#]
         (let [~this       this#
               ~prev-props (om.next/-prev-props prev-props# this#)
               ~prev-state (goog.object/get prev-state# "omcljs$previousState")]
           ~@body
           (om.next/clear-prev-props! this#))))
    'componentWillMount
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this    this#
               indexer# (get-in (om.next/get-reconciler this#) [:config :indexer])]
           (when-not (nil? indexer#)
             (om.next.protocols/index-component! indexer# this#))
           ~@body)))
    'componentWillUnmount
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this    this#
               r#       (om.next/get-reconciler this#)
               cfg#     (:config r#)
               st#      (:state cfg#)
               indexer# (:indexer cfg#)]
           (when (and (not (nil? st#))
                      (get-in @st# [:om.next/queries this#]))
             (swap! st# update-in [:om.next/queries] dissoc this#))
           (when-not (nil? indexer#)
             (om.next.protocols/drop-component! indexer# this#))
           ~@body)))
    'render
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this this#]
           (binding [om.next/*reconciler* (om.next/get-reconciler this#)
                     om.next/*depth*      (inc (om.next/depth this#))
                     om.next/*shared*     (om.next/shared this#)
                     om.next/*instrument* (om.next/instrument this#)
                     om.next/*parent*     this#]
            ~@body))))}
   :defaults
   `{~'isMounted
     ([this#]
       (boolean
         (goog.object/getValueByKeys this#
           "_reactInternalInstance" "_renderedComponent")))
     ~'shouldComponentUpdate
     ([this# next-props# next-state#]
      (let [next-children# (. next-props# -children)
            next-props# (goog.object/get next-props# "omcljs$value")
            next-props# (cond-> next-props#
                          (instance? om.next/OmProps next-props#) om.next/unwrap)]
        (or (not= (om.next/props this#)
                  next-props#)
            (and (.. this# ~'-state)
                 (not= (goog.object/get (. this# ~'-state) "omcljs$state")
                       (goog.object/get next-state# "omcljs$state")))
            (not= (.. this# -props -children)
                  next-children#))))
     ~'componentWillUpdate
     ([this# next-props# next-state#]
      (when (cljs.core/implements? om.next/Ident this#)
        (let [ident# (om.next/ident this# (om.next/props this#))
              next-ident# (om.next/ident this# (om.next/-next-props next-props# this#))]
          (when (not= ident# next-ident#)
            (let [idxr# (get-in (om.next/get-reconciler this#) [:config :indexer])]
              (when-not (nil? idxr#)
                (swap! (:indexes idxr#)
                  (fn [indexes#]
                    (-> indexes#
                      (update-in [:ref->components ident#] disj this#)
                      (update-in [:ref->components next-ident#] (fnil conj #{}) this#)))))))))
       (om.next/merge-pending-props! this#)
       (om.next/merge-pending-state! this#))
     ~'componentDidUpdate
     ([this# prev-props# prev-state#]
       (om.next/clear-prev-props! this#))
     ~'componentWillMount
     ([this#]
       (let [indexer# (get-in (om.next/get-reconciler this#) [:config :indexer])]
         (when-not (nil? indexer#)
           (om.next.protocols/index-component! indexer# this#))))
     ~'componentWillUnmount
     ([this#]
       (let [r#       (om.next/get-reconciler this#)
             cfg#     (:config r#)
             st#      (:state cfg#)
             indexer# (:indexer cfg#)]
         (when (and (not (nil? st#))
                    (get-in @st# [:om.next/queries this#]))
           (swap! st# update-in [:om.next/queries] dissoc this#))
         (when-not (nil? indexer#)
           (om.next.protocols/drop-component! indexer# this#))))}})

(defn reshape [dt {:keys [reshape defaults]}]
  (letfn [(reshape* [x]
            (if (and (sequential? x)
                     (contains? reshape (first x)))
              (let [reshapef (get reshape (first x))]
                (validate-sig x)
                (reshapef x))
              x))
          (add-defaults-step [ret [name impl]]
            (if-not (some #{name} (map first (filter seq? ret)))
              (let [[before [p & after]] (split-with (complement '#{Object}) ret)]
                (into (conj (vec before) p (cons name impl)) after))
              ret))
          (add-defaults [dt]
            (reduce add-defaults-step dt defaults))
          (add-object-protocol [dt]
            (if-not (some '#{Object} dt)
              (conj dt 'Object)
              dt))]
    (->> dt (map reshape*) vec add-object-protocol add-defaults)))

#?(:clj
   (defn- add-proto-methods* [pprefix type type-sym [f & meths :as form]]
     (let [pf (str pprefix (name f))
           emit-static (when (-> type-sym meta :static)
                         `(~'js* "/** @nocollapse */"))]
       (if (vector? (first meths))
         ;; single method case
         (let [meth meths]
           [`(do
               ~emit-static
               (set! ~(#'cljs.core/extend-prefix type-sym (str pf "$arity$" (count (first meth))))
                 ~(with-meta `(fn ~@(#'cljs.core/adapt-proto-params type meth)) (meta form))))])
         (map (fn [[sig & body :as meth]]
                `(do
                   ~emit-static
                   (set! ~(#'cljs.core/extend-prefix type-sym (str pf "$arity$" (count sig)))
                     ~(with-meta `(fn ~(#'cljs.core/adapt-proto-params type meth)) (meta form)))))
           meths)))))

#?(:clj (intern 'cljs.core 'add-proto-methods* add-proto-methods*))

#?(:clj
   (defn- proto-assign-impls [env resolve type-sym type [p sigs]]
     (#'cljs.core/warn-and-update-protocol p type env)
     (let [psym      (resolve p)
           pprefix   (#'cljs.core/protocol-prefix psym)
           skip-flag (set (-> type-sym meta :skip-protocol-flag))
           static?   (-> p meta :static)
           type-sym  (cond-> type-sym
                       static? (vary-meta assoc :static true))
           emit-static (when static?
                         `(~'js* "/** @nocollapse */"))]
       (if (= p 'Object)
         (#'cljs.core/add-obj-methods type type-sym sigs)
         (concat
           (when-not (skip-flag psym)
             [`(do
                 ~emit-static
                 (set! ~(#'cljs.core/extend-prefix type-sym pprefix) true))])
           (mapcat
             (fn [sig]
               (if (= psym 'cljs.core/IFn)
                 (#'cljs.core/add-ifn-methods type type-sym sig)
                 (#'cljs.core/add-proto-methods* pprefix type type-sym sig)))
             sigs))))))

#?(:clj (intern 'cljs.core 'proto-assign-impls proto-assign-impls))

(defn defui*
  ([name form] (defui* name form nil))
  ([name forms env]
   (letfn [(field-set! [obj [field value]]
             `(set! (. ~obj ~(symbol (str "-" field))) ~value))]
     (let [{:keys [dt statics]} (collect-statics forms)
           _ (validate-statics dt)
           rname (if env
                   (:name (ana/resolve-var (dissoc env :locals) name))
                   name)
           ctor  `(defn ~(with-meta name
                           (merge {:jsdoc ["@constructor"]}
                             (when (-> name meta :private)
                               {:private true})))
                    []
                    (this-as this#
                      (.apply js/React.Component this# (js-arguments))
                      (if-not (nil? (.-initLocalState this#))
                        (set! (.-state this#) (.initLocalState this#))
                        (set! (.-state this#) (cljs.core/js-obj)))
                      this#))
           set-react-proto! `(set! (.-prototype ~name)
                                 (goog.object/clone js/React.Component.prototype))
           ctor  (if (-> name meta :once)
                   `(when-not (cljs.core/exists? ~name)
                      ~ctor
                      ~set-react-proto!)
                   `(do
                      ~ctor
                      ~set-react-proto!))
           display-name (if env
                          (str (-> env :ns :name) "/" name)
                          'js/undefined)]
       `(do
          ~ctor
          (specify! (.-prototype ~name) ~@(reshape dt reshape-map))
          (set! (.. ~name -prototype -constructor) ~name)
          (set! (.. ~name -prototype -constructor -displayName) ~display-name)
          (set! (.. ~name -prototype -om$isComponent) true)
          ~@(map #(field-set! name %) (:fields statics))
          (specify! ~name
            ~@(mapv #(cond-> %
                       (symbol? %) (vary-meta assoc :static true)) (:protocols statics)))
          (specify! (. ~name ~'-prototype) ~@(:protocols statics))
          (set! (.-cljs$lang$type ~rname) true)
          (set! (.-cljs$lang$ctorStr ~rname) ~(str rname))
          (set! (.-cljs$lang$ctorPrWriter ~rname)
            (fn [this# writer# opt#]
              (cljs.core/-write writer# ~(str rname)))))))))

(defmacro defui [name & forms]
  (defui* name forms &env))

(defmacro ui
  [& forms]
  (let [t (with-meta (gensym "ui_") {:anonymous true})]
    `(do (defui ~t ~@forms) ~t)))

(defn invariant*
  [condition message env]
  (let [opts (ana-api/get-options)
        fn-scope (:fn-scope env)
        fn-name (some-> fn-scope first :name str)]
    (when-not (:elide-asserts opts)
      `(let [l# om.next/*logger*]
         (when-not ~condition
           (goog.log/error l#
             (str "Invariant Violation"
               (when-not (nil? ~fn-name)
                 (str " (in function: `" ~fn-name "`)"))
               ": " ~message)))))))

(defmacro invariant
  [condition message]
  (invariant* condition message &env))

(comment
  (collect-statics
    '(static IFoo
       (foo [_])
       (bar [_])
       static field sel '[:woz ?noz]
       Object
       (toString [_])))

  (require '[clojure.pprint :refer [pprint]])

  (pprint
    (defui* 'Artist
     '(static IFoo
        (foo [_])
        (bar [_])
        static field sel '[:woz ?noz]
        Object
        (toString [_]))))

  (pprint
    (defui* 'Artist
      '(static IFoo
         (foo [_])
         (bar [_])
         static field sel '[:woz ?noz]
         Object
         (render [_ {:keys [self artists]}]
           (om.dom/div nil "Hello!"))
         (toString [_]))))

  (pprint
    (defui* 'Artist
      '(static IFoo
         (foo [_])
         (bar [_])
         static field sel '[:woz ?noz]
         Object
         (render [_ {:keys [self artists]}]
           (om.dom/div nil "Hello!"))
         (componentWillUnmount [this]
           (first [1 2 3])))))

  (pprint
    (defui* 'Artist
      '(Object
        (initLocalState [this]
          {:foo 'bar})
        (render [_ {:keys [self artists]}]
          (om.dom/div nil "Hello!")))))

  (pprint
    (defui* 'Component
     '(static om.next/IQuery
        (query [this] '[:foo/bar :baz/woz]))))
  )
