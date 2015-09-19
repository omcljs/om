(ns om.next.tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [om.next :as om :refer-macros [defui]]
            [om.next.protocols :as p]
            [om.dom :as dom]))

;; -----------------------------------------------------------------------------
;; Components

(defui Component
  static om/IQuery
  (query [this]
    '[:foo/bar :baz/woz])
  Object
  (render [this]))

(defui ComponentList
  static om/IQueryParams
  (params [this]
    {:component (om/get-query Component)})
  static om/IQuery
  (query [this]
    '[{:components/list ?component} :app/title])
  Object
  (render [this]))

(deftest test-component?
  (is (om/component? (Component. {}))))

;; -----------------------------------------------------------------------------
;; Queries

(deftest test-query
  (is (= (om/query Component)
         '[:foo/bar :baz/woz]))
  (is (= (om/query ComponentList)
         '[{:components/list ?component} :app/title])))

(deftest test-get-query
  (is (= (om/get-query Component)
         '[:foo/bar :baz/woz]))
  (is (= (om/get-query ComponentList)
         '[{:components/list [:foo/bar :baz/woz]} :app/title])))

;; -----------------------------------------------------------------------------
;; Indexer

(deftest test-indexer
  (let [idxr (om/indexer identity)
        idxs (p/index-root idxr ComponentList)]
    (is (= (set (keys (:prop->classes idxs)))
           #{:app/title :components/list :foo/bar :baz/woz}))
    (is (= (get-in idxs [:class->paths Component])
           #{[:components/list]}))
    (is (= (get-in idxs [:class->selectors Component])
           #{[{:components/list [:foo/bar :baz/woz]}]}))))

(deftest test-reconciler-has-indexer
  (let [r (om/reconciler
            {:state (atom nil)
             :ui->ref identity})]
    (is (instance? om/Indexer (get-in r [:config :indexer])))))

;; -----------------------------------------------------------------------------
;; Refs

(deftest test-ref
  (let [r  (om/ref :foo 0)
        rs (om/refs :foo 0)]
    (is (= :foo (:root r)))
    (is (= 0 (:id r)))
    (is (= :foo (:root (first rs))))
    (is (= 0 (:id (first rs))))))

;; -----------------------------------------------------------------------------
;; Parser

(defmulti prop (fn [env k] k))

(defmethod prop :default
  [env k] {:quote k})

(defmethod prop :foo/bar
  [{:keys [state]} k]
  (if-let [v (get @state k)]
    {:value v}
    {:quote k}))

(def p (om/parser {:prop prop}))

(deftest test-basic-parsing
  (let [st (atom {:foo/bar 1})]
    (is (= (p {} [:baz/woz]) {}))
    (is (= (p {:state st} [:foo/bar]) {:foo/bar 1}))
    (is (= (p {:state st} [:foo/bar :baz/woz]) {:foo/bar 1}))
    (is (= (p {} [:baz/woz] true) [:baz/woz]))
    (is (= (p {:state st} [:foo/bar] true) []))
    (is (= (p {:state st} [:foo/bar :baz/woz] true) [:baz/woz]))))

(comment
  (run-tests)

  (require '[cljs.pprint :as pp])

  (let [idxr (om/indexer identity)]
    (pp/pprint (p/index-root idxr ComponentList)))
  )