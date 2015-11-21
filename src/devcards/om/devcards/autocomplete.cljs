(ns om.devcards.autocomplete
  (:require-macros [devcards.core :refer [defcard deftest]]
                   [cljs.core.async.macros :refer [go]])
  (:require [goog.events :as events]
            [goog.events.EventType]
            [cljs.core.async :as async :refer [<! >! put! chan]]
            [clojure.string :as string]
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
  [{:keys [state ast] :as env} k {:keys [query]}]
  (merge
    {:value (get @state k [])}
    (when-not (string/blank? query)
     {:search ast})))

;; -----------------------------------------------------------------------------
;; App

(defn result-list [results]
  (dom/ul #js {:key "result-list"}
    (map #(dom/li nil %) results)))

(defn search-field [ac query]
  (dom/input
    #js {:key "search-field"
         :value query
         :onKeyUp
         (fn [e]
           (om/set-query! ac
             {:params {:query (.. e -target -value)}}))}))

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
          [(search-field this (:search-query (om/get-params this)))]
          results (conj (result-list results)))))))

(def send-chan (chan))

(defn send-to-chan [c]
  (fn [{:keys [search]} cb]
    (put! c [(-> search (nth 0) (nth 1)) cb])))

(def reconciler
  (om/reconciler
    {:state   {:search/results []}
     :parser  (om/parser {:read read})
     :send    (send-to-chan send-chan)
     :remotes [:remote :search]}))

(defn send-loop [c]
  (go
    (loop [[{:keys [query]} cb] (<! c)]
      (let [raw     (<! (jsonp (str base-url query)))
            results (aget raw 1)]
        (cb {:search/results results}))
      (recur (<! c)))))

(send-loop send-chan)

(defcard test-autocomplete
  "Demonstrate simple autocompleter"
  (om/mock-root reconciler AutoCompleter))

(comment

  (go (println (<! (jsonp (str base-url "do")))))

  (def parser (om/parser {:read read}))

  (parser {} (om/get-query AutoCompleter) :search)

  )
