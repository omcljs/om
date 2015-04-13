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

(defn get-query
  ([cl] (get-query cl :self))
  ([cl k]
   (with-meta
     (bind-query (k (queries cl)) (k (params cl)))
     {:class cl})))

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
      {:self {:tracks (get-query Track)}})
    static IQuery
    (queries [this]
      '{:self [:album/name ?tracks]}))

  (.render (Track. nil nil nil))

  (get-query Track)
  (get-query Track :artists)
  (get-query AlbumTracks)

  (-> (get-query AlbumTracks) meta :class)
  (-> (get-query AlbumTracks) second meta :class)

  )
