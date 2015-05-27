(ns om.next
  (:refer-clojure :exclude [var? key])
  (:require-macros [om.next :refer [defui]])
  (:require [goog.string :as gstring]
            [clojure.walk :as walk]
            [om.next.protocols :as p]
            [om.next.stores :refer [TreeStore]]))

;; =============================================================================
;; Globals & Dynamics

(def ^{:dynamic true :private true} *app-state* nil)

(def ^:private render-queued false)

(def ^:private render-queue (atom #{}))

;; =============================================================================
;; User Protocols

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

(defn create-factory [cl]
  (fn [props children]
    (js/React.createElement cl
      #js {:omcljs$value props
           :omcljs$appState *app-state*}
      children)))

(defn props [c]
  (.. c -props -omcljs$value))

(defn state [c]
  (.-state c))

(defn app-state [c]
  (.. c -props -omcljs$appState))

(defn key [c]
  (.. c -props -key))

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

(defn needs-display! [xs]
  (swap! render-queue into xs))

(defn commit! [c entity]
  (let [store @(app-state c)
        [store' render-list] (p/commit store c entity)]
    (reset! (app-state c) store')
    (needs-display! render-list)))

(defn flush-queue []
  (doseq [c @render-queue]
    (.forceUpdate c))
  (set! render-queued false))

(defn root [class state {:keys [target raf]}]
  (let [ret  (atom nil)]
    (letfn [(render [data]
              (binding [*app-state* state]
                (reset! ret
                  (js/React.render ((create-factory class) data) target))))]
      (let [sel (bound-query class)
            store @state]
        (cond
          (satisfies? p/IPullAsync store) (p/pull-async store sel nil render)
          :else (render (p/pull store sel nil)))
        (add-watch state :om/root
          (fn [_ _ o n]
            (when-not render-queued
              (set! render-queued true)
              (cond
                (fn? raf) (raf)

                (or (false? raf)
                    (not (exists? js/requestAnimationFrame)))
                (js/setTimeout flush-queue 16)

                :else
                (js/requestAnimationFrame flush-queue)))))
        @ret))))

(defn tree-store [root-class data]
  (atom (TreeStore. data (build-index root-class))))