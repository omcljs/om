(ns om.next
  (:refer-clojure :exclude [deftype])
  (:require [cljs.core :refer [deftype specify! this-as js-arguments]]))

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
         (let [store# @(om.next/app-state ~this)]
           (when (satisfies? om.next.protocols/IComponentIndex store#)
             (om.next.protocols/index-component store# ~this)))
         ~@body))
    'componentWillUnmount
    (fn [[name [this :as args] & body]]
      `(~name ~args
         (let [store# @(om.next/app-state ~this)]
           (when (satisfies? om.next.protocols/IComponentIndex store#)
             (om.next.protocols/drop-component store# ~this)))
         ~@body))}
   :defaults
   `{~'shouldComponentUpdate
     ([this# next-props# next-state#]
       (or (not= (om.next/props this#) (.-omcljs$value next-props#))
         (not= (om.next/state this#) next-state#)))
     ~'componentWillMount
     ([this#]
       (let [store# @(om.next/app-state this#)]
         (when (satisfies? om.next.protocols/IComponentIndex store#)
           (om.next.protocols/index-component store# this#))))
     ~'componentWillUnmount
     ([this#]
       (let [store# @(om.next/app-state this#)]
         (when (satisfies? om.next.protocols/IComponentIndex store#)
           (om.next.protocols/drop-component store# this#))))}})

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

(defn defui* [name forms]
  (letfn [(field-set! [[field value]]
            `(set! (. ~name ~(symbol (str "-" field))) ~value))]
    (let [{:keys [dt statics]} (collect-statics forms)]
      `(do
         (defn ~name []
           (this-as this#
             (.apply js/React.Component this# (js-arguments))))
         (set! (.-prototype ~name) (goog.object/clone js/React.Component.prototype))
         (specify! (.-prototype ~name) ~@(reshape dt reshape-map))
         (set! (.. ~name -prototype -constructor) ~name)
         ~@(map field-set! (:fields statics))
         (specify! ~name ~@(:protocols statics))))))

(defmacro defui [name & forms]
  (defui* name forms))

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
