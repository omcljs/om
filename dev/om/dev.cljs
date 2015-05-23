(ns om.dev
  (:refer-clojure :exclude [var?])
  (:require [clojure.browser.repl :as repl]
            [om.next :as next :refer-macros [defui]]
            [om.core :as om]
            [om.dom :as dom]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(comment
  (next/tree-pull {:foo 1 :bar 2} [:foo] nil nil)
  )

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

(comment
  (next/tree-pull
   {:track/name "Cool song"
    :track/artists [0 2]}
   [:track/name {:track/artists [:artist/name]}]
   db #{:track/artists})
  )

(deftype TreeQuery [foreign-keys]
  next/IQueryEngine
  (-run-query [this db q]
    ))

(deftype TreeStorage []
  next/IStorage
  (-transact [this db xs]
    ))

(comment
  '[:find (pull ?e ?selector)
    :where [?e :album/name "Awesome Album"]]

  (next/bind-query '[:foo (?bar)] {:bar 3})
  )

(defui Artist
  static next/IQuery
  (queries [this]
    '{:self [:artist/name :artist/age]})
  Object
  (render [this props]
    (let [{:keys [:artist/name :artist/age]} (:self props)]
      (dom/div nil
        (dom/div nil
          (dom/label nil "Artist Name:")
          (dom/span nil name))
        (dom/div nil
          (dom/label nil "Artist Age:")
          (dom/span nil age))))))

(def artist (next/create-factory Artist))

(comment
  (next/complete-query Artist)

  (dom/render-to-str
    (artist {:artist/name "Bob Smith"
             :artist/age 27}))
  )

(defui ArtistList
  Object
  (render [this {:keys [props]}]
    (apply dom/ul nil
      (map artist props))))

(def artist-list (next/create-factory ArtistList))

(defui Track
  static next/IQueryParams
  (params [this]
    {:artists {:artist (next/complete-query Artist)}})
  static next/IQuery
  (queries [this]
    '{:self [:track/name]
      :artists [{:track/artists ?artist}]})
  Object
  (render [this props]
    (let [{:keys [self artists]} props]
      (dom/div nil
        (dom/h2 (:track/name self))
        (artist-list artists)))))

(def track (next/create-factory Track))

(defui AlbumTracks
  static next/IQueryParams
  (params [this]
    {:self {:tracks (next/complete-query Track)}})
  static next/IQuery
  (queries [this]
    '{:self [:album/name {:album/tracks ?tracks}]})
  Object
  (render [this props]
    (let [{:keys [:album/name :album/tracks]} (:self props)]
      (apply dom/div nil
       (dom/h1 nil name)
       (map Track tracks)))))

(comment
  (next/get-query Track)
  (next/get-query Track :artists)
  (next/get-query AlbumTracks)

  (-> (next/complete-query Track) meta)

  (next/bind-props* Track
    {:track/name "Foo"
     :track/artists [{:artist/name "Bob" :artist/age 27}]})

  (next/complete-query AlbumTracks)

  (next/tree-pull
    (get-in db [:albums 1])
    (next/complete-query AlbumTracks)
    db #{:track/artists :album/tracks})

  (time
    (dotimes [_ 1000]
      (next/tree-pull
        (get-in db [:albums 1])
        (next/complete-query AlbumTracks)
        db #{:track/artists :album/tracks})))

  (-> (next/complete-query AlbumTracks) meta)
  (-> (next/complete-query AlbumTracks) second)
  (-> (next/complete-query AlbumTracks) second :album/tracks meta)

  (defn root [root-widget]
    (go (let [d (<! (pull (complete-query root-widget)))]
          (js/React.renderComponent (root-widget d)
            (gdom/getElement "App")))))
  )
