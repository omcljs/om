(ns om.next
  (:refer-clojure :exclude [deftype])
  (:require [cljs.core :refer [deftype specify! this-as js-arguments]]
            [cljs.analyzer :as ana]))

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
              :else (throw (IllegalArgumentException. "Malformed static")))
            (recur nil dt' statics)))
        {:dt dt' :statics statics}))))

(def reshape-map
  {:reshape
   {'componentWillMount
    (fn [[name [this :as args] & body]]
      `(~name ~args
         (let [reconciler# (om.next/reconciler ~this)]
           (when (satisfies? om.next.protocols/IComponentIndex reconciler#)
             (om.next.protocols/index-component! reconciler# ~this)))
         ~@body))
    'componentWillUnmount
    (fn [[name [this :as args] & body]]
      `(~name ~args
         (let [reconciler# (om.next/reconciler ~this)]
           (when (satisfies? om.next.protocols/IComponentIndex reconciler#)
             (om.next.protocols/drop-component! reconciler# ~this)))
         ~@body))
    'render
    (fn [[name [this :as args] & body]]
      `(~name ~args
         (binding [om.next/*reconciler* (om.next/reconciler ~this)
                   om.next/*root-class* (om.next/root-class ~this)
                   om.next/*depth*      (om.next/depth ~this)
                   om.next/*parent*     ~this]
           ~@body)))}
   :defaults
   `{~'shouldComponentUpdate
     ([this# next-props# next-state#]
       (or (not= (om.next/props this#) (.-omcljs$value next-props#))
           (not= (om.next/state this#) next-state#)))
     ~'componentWillMount
     ([this#]
       (let [reconciler# (om.next/reconciler this#)]
         (when (satisfies? om.next.protocols/IComponentIndex reconciler#)
           (om.next.protocols/index-component! reconciler# this#))))
     ~'componentWillUnmount
     ([this#]
       (let [reconciler# (om.next/reconciler this#)]
         (when (satisfies? om.next.protocols/IComponentIndex reconciler#)
           (om.next.protocols/drop-component! reconciler# this#))))}})

(defn reshape [dt {:keys [reshape defaults]}]
  (letfn [(reshape* [x]
            (if (and (sequential? x)
                     (contains? reshape (first x)))
              ((get reshape (first x)) x)
              x))
          (add-defaults-step [ret [name impl]]
            (if-not (some #{name} (map first (filter seq? ret)))
              (let [[before [p & after]] (split-with (complement '#{Object}) ret)]
                (into (conj (vec before) p (cons name impl)) after))
              ret))
          (add-defaults [dt]
            (reduce add-defaults-step dt defaults))]
    (->> dt (map reshape*) vec add-defaults)))

(defn defui* [name forms env]
  (letfn [(field-set! [[field value]]
            `(set! (. ~name ~(symbol (str "-" field))) ~value))]
    (let [{:keys [dt statics]} (collect-statics forms)
          rname (:name (ana/resolve-var (dissoc env :locals) name))]
      `(do
         (defn ~name []
           (this-as this#
             (.apply js/React.Component this# (js-arguments))))
         (set! (.-prototype ~name) (goog.object/clone js/React.Component.prototype))
         (specify! (.-prototype ~name) ~@(reshape dt reshape-map))
         (set! (.. ~name -prototype -constructor) ~name)
         ~@(map field-set! (:fields statics))
         (specify! ~name ~@(:protocols statics))
         (set! (.-cljs$lang$type ~rname) true)
         (set! (.-cljs$lang$ctorStr ~rname) ~(str rname))
         (set! (.-cljs$lang$ctorPrWriter ~rname)
           (fn [this# writer# opt#]
             (cljs.core/-write writer# ~(str rname))))))))

(defmacro defui [name & forms]
  (defui* name forms &env))

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
  )
