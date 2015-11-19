(ns om.devcards.autocomplete
  (:require-macros [devcards.core :refer [defcard deftest]]
                   [cljs.core.async.macros :refer [go]])
  (:require [goog.events :as events]
            [goog.events.EventType]
            [cljs.core.async :as async :refer [<! >! put! chan]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom])
  (:import [goog Uri]
           [goog.net Jsonp]))

(def base-url
  "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=")

(defn jsonp
  ([uri] (jsonp (chan) uri))
  ([c uri]
   (let [gjsonp (Jsonp. (Uri. uri))]
     (.send gjsonp nil #(put! c %))
     c)))

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
      (cond->
        [(search-field (:search-query (om/get-params this)))]
        results (conj (result-list results))))))

(defn send [{:keys [remote]} cb]
  )

(def reconciler
  (om/reconciler
    {:state {}
     :send  send}))

(defcard test-autocomplete
  "Demonstrate simple autocompleter")
