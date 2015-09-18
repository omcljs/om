(ns om.next.tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [om.next :as om :refer-macros [defui]]
            [om.next.protocols :as p]
            [om.dom :as dom]))

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

(deftest test-indexer
  (let [idxr (om/indexer identity)
        idxs (p/index-root idxr ComponentList)]
    (is (= (set (keys (:prop->component idxs)))
           #{:app/title :components/list :foo/bar :baz/woz}))
    (is (= (get-in idxs [:component->path Component])
           [:components/list]))
    (is (= (get-in idxs [:component->selector Component])
           [{:components/list [:foo/bar :baz/woz]}]))))

(deftest test-reconciler-has-indexer
  (let [r (om/reconciler
            {:state (atom nil)
             :ui->ref identity})]
    (is (instance? om/Indexer (get-in r [:config :indexer])))))

(deftest test-ref
  (let [r  (om/ref :foo 0)
        rs (om/refs :foo 0)]
    (is (= :foo (:root r)))
    (is (= 0 (:id r)))
    (is (= :foo (:root (first rs))))
    (is (= 0 (:id (first rs))))))

(comment
  (run-tests)

  (require '[cljs.pprint :as pp])

  (let [idxr (om/indexer identity)]
    (pp/pprint (p/index-root idxr ComponentList)))
  )