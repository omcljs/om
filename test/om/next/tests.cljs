(ns om.next.tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [om.next :as om :refer-macros [defui]]))

(defui Component
  static om/IQuery
  (queries [this]
    {:self [:foo/bar :baz/woz]})
  Object
  (render [this]))

(defui ComponentList
  static om/IQueryParams
  (params [this]
    {:components {:component (om/complete-query Component)}})
  static om/IQuery
  (queries [this]
    '{:components ?component})
  Object
  (render [this]))

(comment
  (om/get-query Component)
  (om/get-query ComponentList)
  )