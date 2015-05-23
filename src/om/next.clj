(ns om.next
  (:refer-clojure :exclude [deftype])
  (:require [cljs.core :refer [deftype specify!]]))

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

(defn reshape [dt reshape-map]
  (letfn [(reshape* [x]
            (if (and (sequential? x)
                     (contains? reshape-map (first x)))
              ((get reshape-map (first x)) x)
              x))]
    (vec (map reshape* dt))))

(defn defui* [name forms]
  (letfn [(field-set! [[field value]]
            `(set! (. ~name ~(symbol (str "-" field))) ~value))]
    (let [{:keys [dt statics]} (collect-statics forms)]
      `(do
         (deftype ~name [~'props ~'children ~'opts]
           ~@(reshape dt {}))
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
  )
