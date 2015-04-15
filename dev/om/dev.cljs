(ns om.dev
  (:require-macros [om.dev :refer [defui]])
  (:require [goog.string :as gstring]
            [clojure.browser.repl :as repl]
            [om.core :as om]
            [om.dom :as dom]
            [clojure.walk :as walk]
            [clojure.data :as data])
  (:import [goog.i18n MessageFormat]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(defprotocol IQueryParams
  (params [this]))

(defprotocol IQuery
  (queries [this]))

(defprotocol IQueryEngine
  (-run-query [this db q]))

(defprotocol IStorage
  (-transact [this db xs]))

(defn run-query [x db q]
  (-run-query x db q))

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

(defn complete-query [cl]
  (letfn [(bind [k]
            (bind-query (k (queries cl)) (k (params cl))))
          (key-repeat [k]
            (repeat (count (k (queries cl))) k))
          (key-order [ks]
            (vec (mapcat key-repeat ks)))]
    (let [qs (queries cl)
          qks (keys qs)
          bound-qs (map bind qks)]
      (with-meta
        (reduce into (first bound-qs) (rest bound-qs))
        {:class cl :key-order (key-order qks)}))))

(defn tree-pull [x selector db fks]
  (loop [selector (seq selector) ret {}]
    (if selector
      (let [k (first selector)]
        (cond
          (keyword? k)
          (recur (next selector) (assoc ret k (get x k)))
          (map? k)
          (recur (next selector)
            (let [[k' selector'] (first k)
                  ys (if (contains? fks k')
                       (let [table (keyword (name k'))]
                         (map (get db table) (get x k')))
                       (get x k'))]
              (assoc ret
                k'
                (vec (map #(tree-pull % selector' db fks) ys)))))))
      ret)))

(comment
  (tree-pull {:foo 1 :bar 2} [:foo] nil nil)

  (def db
    {:artist
     {0 {:artist/name "Bobby Bob" :artist/age 27}
      1 {:artist/name "Susie Susie" :artist/age 30}
      2 {:artist/name "Johnny Jon" :artist/age 21}}})

  (tree-pull
    {:track/name "Cool song"
     :track/artist [0 2]}
    [:track/name {:track/artist [:artist/name]}]
    db #{:track/artist})
  )

(deftype TreeQuery [foreign-keys]
  IQueryEngine
  (-run-query [this db q]
    ))

(deftype TreeStorage []
  IStorage
  (-transact [this db xs]
    ))

(comment
  ;; db at top-level, entities
  (def db
    {:album  {0 {:album/name "Awesome Album"
                 :album/tracks [{:track/name "Awesome Track"
                                 :track/artists [0]}]}}
     :track  {0 {:track/name "Awesome Track"
                  :track/artists [0]}}
     :artist {0 {:db/id 0 :artist/name "Bob Smith"}}})

  '[:find (pull ?e ?selector)
    :where [?e :album/name "Awesome Album"]]

  (MessageFormat. "album")

  (def fks
    {:album/tracks  :tracks
     :track/artists :artists})

  (bind-query '[:foo (?bar)] {:bar 3})

  (defui Artist
    static field sel '[:db/id :artist/name :artist/dob]
    Object
    (render [this]
      (dom/div nil "Hello")))

  (js/React.renderToString
    ((js/React.createFactory Artist) nil))

  (defui Artist
    static IQueryParams
    (params [this]
      {:artists {:artist Artist.sel}})
    static IQuery
    (queries [this]
      '{:self [:db/id :track/name]
        :artists [{:track/artists ?artist}]})
    Object
    (render [{:keys [props]}]
      (let [{:keys [self artists]} props]
        (dom/div nil
          (dom/h2 (:track/name self))
          (artist-list artists)
          ))))

  (def artist (js/React.createFactory artist))

  (update-query (artist) (fn [q]))

  (defui ArtistList
    Object
    (render [{:keys [props]}]
      (apply dom/ul nil
        (map artist props))))

  (defui Track
    static IQueryParams
    (params [this]
      {:artists {:artist Artist.sel}})
    static IQuery
    (queries [this]
      `{:self [:db/id :track/name]
        :artists [{:track/artists (foo )}]})
    Object
    (render [{:keys [props]}]
      (let [{:keys [self artists]} props]
        (dom/div nil
          (dom/h2 (:track/name self))
          (artist-list artists)
          ))))

  (defui AlbumTracks
    static IQueryParams
    (params [this]
      {:self {:tracks (complete-query Track)}})
    static IQuery
    (queries [this]
      '{:self [:album/name {:album/tracks ?tracks}]}))

  (.render (Track. nil nil nil))

  (data/diff
    (:artists (queries Track))
    (bind-query
     (conj (:artists (queries Track)) '?something)
     {:artist "foo" :something 1}))

  (get-query Track)
  (get-query Track :artists
    )
  (get-query AlbumTracks)

  (complete-query AlbumTracks)
  (-> (complete-query AlbumTracks) meta)
  (-> (complete-query AlbumTracks) second)
  (-> (complete-query AlbumTracks) second :album/tracks meta)

  (defn root [root-widget]
    (go (let [d (<! (pull (complete-query root-widget)))]
          (js/React.renderComponent (root-widget d)
            (gdom/getElement "App")))))

  )
