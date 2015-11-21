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

(enable-console-print!)

(def base-url
  "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=")

(defn jsonp
  ([uri] (jsonp (chan) uri))
  ([c uri]
   (let [gjsonp (Jsonp. (Uri. uri))]
     (.send gjsonp nil #(put! c %))
     c)))

;; -----------------------------------------------------------------------------
;; Parsing

(defmulti read om/dispatch)

(defmethod read :search/results
  [{:keys [state ast] :as env} _ _]
  (let [st @state]
    (when-not (contains? st :search/results)
      {:value  []
       :search ast})))

;; -----------------------------------------------------------------------------
;; App

(defn result-list [results]
  (dom/ul #js {:key "result-list"}
    (map #(dom/li nil %) results)))

(defn search-field [query]
  (dom/input
    #js {:key "search-field"
         :value query}))

(defui AutoCompleter
  static om/IQueryParams
  (params [_]
    {:query ""})
  static om/IQuery
  (query [_]
    '[(:search/results {:query ?query})])
  Object
  (render [this]
    (let [{:keys [search/results]} (om/props this)]
      (dom/div nil
        (dom/h2 nil "Autocompleter")
        (cond->
          [(search-field (:search-query (om/get-params this)))]
          results (conj (result-list results)))))))

(defn send [{:keys [search]} cb]
  (println search)
  #_(go
    (let [[_ res] (<! (jsonp (str base-url "do")))]
      (cb res))))

(def reconciler
  (om/reconciler
    {:state   {}
     :parser  (om/parser {:read read})
     :send    send
     :remotes [:remote :search]}))

(defcard test-autocomplete
  "Demonstrate simple autocompleter"
  (om/mock-root reconciler AutoCompleter))

(comment

  (go (println (<! (jsonp (str base-url "do")))))

  (def parser (om/parser {:read read}))

  (parser {} (om/get-query AutoCompleter) :search)

  )
