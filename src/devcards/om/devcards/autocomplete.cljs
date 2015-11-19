(ns om.devcards.autocomplete
  (:require-macros [devcards.core :refer [defcard deftest]])
  (:require [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defn result-list [results]
  (dom/ul
    (map #(dom/li nil %) results)))

(defn search-field [query]
  (dom/input
    #js {:value query}))

(defui AutoCompleter
  static om/IQueryParams
  (params [_]
    {:search-query ""})
  static om/IQuery
  (query [_]
    '[(:search/results ?search-query)])
  Object
  (render [this]
    (let [{:keys [search/results]} (om/props this)]
      (cond-> [(search-field (:search-query (om/get-params this)))]
        results (conj (result-list results))))))

(defcard test-autocomplete
  "Demonstrate simple autocompleter")
