(ns om.devcards.core
  (:require-macros [devcards.core :refer [defcard deftest]])
  (:require [cljs.test :refer-macros [is async]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(defui Hello
  Object
  (render [this]
    (dom/p nil (-> this om/props :text))))

(def hello (om/create-factory Hello))

(defcard simple-component
  "Test that Om Next component work as regular React components."
  (hello {:text "Hello, world!"}))

(def p
  (om/parser
    {:read   (fn [_ _ _] {:quote true})
     :mutate (fn [_ _ _] {:quote true})}))

(def r
  (om/reconciler
    {:parser p
     :ui->ref (fn [c] (-> c om/props :id))}))

(defui Binder
  Object
  (componentDidMount [this]
    (let [indexes @(get-in (-> this om/props :reconciler) [:config :indexer])]
      (om/update-state! this assoc :indexes indexes)))
  (render [this]
    (binding [om/*reconciler* (-> this om/props :reconciler)]
      (apply dom/div nil
        (hello {:id 0 :text "Goodbye, world!"})
        (when-let [indexes (get-in (om/get-state this)
                             [:indexes :ref->components])]
          [(dom/p nil (pr-str indexes))])))))

(def binder (om/create-factory Binder))

(defcard basic-nested-component
  "Test that component nesting works"
  (binder {:reconciler r}))

(deftest test-indexer
  "Test indexer"
  (let [idxr (get-in r [:config :indexer])]
    (is (not (nil? idxr)) "Indexer is not nil in the reconciler")
    (is (not (nil? @idxr)) "Indexer is IDeref")))
