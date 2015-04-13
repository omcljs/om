(ns om.dev
  (:require-macros [om.dev :refer [defui]])
  (:require [goog.string :as gstring]
            [clojure.browser.repl :as repl]
            [om.core :as om]
            [om.dom :as dom]
            [clojure.walk :as walk]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(defprotocol IQueryParams
  (params [this]))

(defprotocol IQuery
  (queries [this]))

(defprotocol IQueryEngine
  (run-query [this query]))

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

(defn build-query [x k]
  (bind-query (k (queries x)) (k (params x))))

(defn pull [x selector])

(comment
  (bind-query '[:foo (?bar)] {:bar 3})

  (defui Artist
    static field sel '[:db/id :artist/name]
    Object
    (render [this]))

  (defui Track
    static IQueryParams
    (params [this]
      {:artists {:artist Artist.sel}})
    static IQuery
    (queries [this]
      '{:self [:db/id :track/name]
        :artists [{:track/artists ?artist}]})
    Object
    (render [this]))

  (defui AlbumTracks
    static IQueryParams
    (params [this]
      {:tracks (:self (queries Track))})
    static IQuery
    (queries [this]
      '{:tracks [:album/name ?tracks]}))

  (.render (Track. nil nil nil))

  (build-query Track :self)
  (build-query Track :artists)

  )
