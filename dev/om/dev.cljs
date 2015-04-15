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
    {:albums
     {0 {:album/name "Rock Rock" :album/tracks [0 1 2]}
      1 {:album/name "Medley Muddle" :album/tracks [3 4 5]}}
     :tracks
     {0 {:track/name "Awesome Song No. 1" :track/artists [0 2]}
      1 {:track/name "Awesome Song No. 2" :track/artists [1 2]}
      2 {:track/name "Awesome Song No. 3" :track/artists [2 5]}
      3 {:track/name "Ballad No. 1" :track/artists [1 2]}
      4 {:track/name "Pop Hit No. 5" :track/artists [3 4]}
      5 {:track/name "Punk Rock No. 1" :track/artists [1 5]}}
     :artists
     {0 {:artist/name "Bobby Bob" :artist/age 27}
      1 {:artist/name "Susie Susie" :artist/age 30}
      2 {:artist/name "Johnny Jon" :artist/age 21}
      3 {:artist/name "Jimmy Jo" :artist/age 40}
      4 {:artist/name "Peter Pop" :artist/age 19}
      5 {:artist/name "Betty Blues" :artist/age 50}}})

  (tree-pull
    {:track/name "Cool song"
     :track/artists [0 2]}
    [:track/name {:track/artists [:artist/name]}]
    db #{:track/artists})
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
  '[:find (pull ?e ?selector)
    :where [?e :album/name "Awesome Album"]]

  (bind-query '[:foo (?bar)] {:bar 3})

  (defui Artist
    static field sel '[:artist/name :artist/age]
    Object
    (render [this]
      (dom/div nil "Hello")))

  (def artist (js/React.createFactory artist))

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
      `{:self [:track/name]
        :artists [{:track/artists ?artist}]})
    Object
    (render [{:keys [props]}]
      (let [{:keys [self artists]} props]
        (dom/div nil
          (dom/h2 (:track/name self))))))

  (defui AlbumTracks
    static IQueryParams
    (params [this]
      {:self {:tracks (complete-query Track)}})
    static IQuery
    (queries [this]
      '{:self [:album/name {:album/tracks ?tracks}]}))

  (get-query Track)
  (get-query Track :artists)
  (get-query AlbumTracks)

  (complete-query AlbumTracks)

  (tree-pull
    (get-in db [:albums 1])
    (complete-query AlbumTracks)
    db #{:track/artists :album/tracks})

  (-> (complete-query AlbumTracks) meta)
  (-> (complete-query AlbumTracks) second)
  (-> (complete-query AlbumTracks) second :album/tracks meta)

  (defn root [root-widget]
    (go (let [d (<! (pull (complete-query root-widget)))]
          (js/React.renderComponent (root-widget d)
            (gdom/getElement "App")))))

  )
