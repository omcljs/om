(ns om.next.tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [om.next :as om :refer-macros [defui]]))

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
    ;; '{:components ?component/self}
    '[{:components/list ?component}])
  Object
  (render [this]))

(comment
  (om/queries Component)
  (om/get-query Component)
  (om/get-query ComponentList :components)
  (om/queries ComponentList)
  )