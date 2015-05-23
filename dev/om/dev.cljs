(ns om.dev
  (:refer-clojure :exclude [var?])
  (:require [clojure.browser.repl :as repl]
            [om.next :as next :refer-macros [defui]]
            [om.dom :as dom]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

#_(defn hello [app owner]
  (reify
    om/IDidMount
    (did-mount [this]
      (println (gdomdata/get (om/get-node owner) "reactid")))
    om/IRender
    (render [this]
      (dom/div #js {:key "root"} "Hello world!"
        (dom/ul #js {:key "yow"}
          (dom/li #js {:key "foo"} "foo")
          (dom/li #js {:key "bar"} "bar")
          (dom/li #js {:key "baz"} "baz"))))))

#_(om/root hello {}
  {:target (gdom/getElement "app")})

(comment
  (next/tree-pull {:foo 1 :bar 2} [:foo] nil nil)

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
      '{:self [:track/name]
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

  (time
    (dotimes [_ 1000]
      (tree-pull
        (get-in db [:albums 1])
        (complete-query AlbumTracks)
        db #{:track/artists :album/tracks})))

  (-> (complete-query AlbumTracks) meta)
  (-> (complete-query AlbumTracks) second)
  (-> (complete-query AlbumTracks) second :album/tracks meta)

  (defn root [root-widget]
    (go (let [d (<! (pull (complete-query root-widget)))]
          (js/React.renderComponent (root-widget d)
            (gdom/getElement "App")))))

  )
