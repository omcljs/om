(ns om.dev
  (:refer-clojure :exclude [var?])
  (:require [clojure.browser.repl :as repl]
            [om.next :as om :refer-macros [defui]]
            [om.next.protocols :as p]
            [om.dom :as dom]
            [goog.object :as gobj]
            [goog.dom :as gdom]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(defn increment! [c props]
  (om/assert! c (update-in props [:counter/count] inc)))

(defui Counter
  static om/IQuery
  (query [this]
    '[:id :counter/count])
  Object
  (render [this]
    (let [{:keys [:counter/count] :as props} (om/props this)]
      (dom/div nil
        (dom/p nil (str "Count: " count))
        (dom/button
          #js {:onClick (fn [_] (increment! this props))}
          "Click Me!")))))

(def counter (om/create-factory Counter))

(def current-id (atom 2))

(defn add-counter! [c]
  (let [id (swap! current-id inc)]
    (om/assert! c
      (update-in (om/props c) [:app/counters]
        conj {:id id :counter/count 0}))))

(defui HelloWorld
  static om/IQueryParams
  (params [this]
    {:counter (om/get-query Counter)})
  static om/IQuery
  (query [this]
    '[:app/title {:app/counters ?counter}])
  Object
  (render [this]
    (let [{:keys [:app/title :app/counters] :as props} (om/props this)]
      (apply dom/div nil
        (dom/h2 nil title)
        (dom/div nil
          (dom/button
            #js {:onClick (fn [e] (add-counter! this))}
            "Add Counter!"))
        (om/map-keys counter :id counters)))))

(def reconciler
  (om/tree-reconciler
    {:app/title "Hello World!"
     :app/counters [{:id 0 :counter/count 0}
                    {:id 1 :counter/count 0}
                    {:id 2 :counter/count 0}]}))

(comment
  (def reconciler
    (om/table-reconciler
      {:app {:app/title "Hello World!" :app/state [0 1 2]}
       :app/state [{:state/count 0}
                   {:state/count 0}
                   {:state/count 0}]}))
  )

(om/add-root! reconciler
  (gdom/getElement "app") HelloWorld)

(comment
  (om/store reconciler)
  (om/basis-t reconciler)
  (p/indexes reconciler)
  )

;(def db
;  {:albums
;   {0 {:album/name "Rock Rock" :album/tracks [0 1 2]}
;    1 {:album/name "Medley Muddle" :album/tracks [3 4 5]}}
;   :tracks
;   {0 {:track/name "Awesome Song No. 1" :track/artists [0 2]}
;    1 {:track/name "Awesome Song No. 2" :track/artists [1 2]}
;    2 {:track/name "Awesome Song No. 3" :track/artists [2 5]}
;    3 {:track/name "Ballad No. 1" :track/artists [1 2]}
;    4 {:track/name "Pop Hit No. 5" :track/artists [3 4]}
;    5 {:track/name "Punk Rock No. 1" :track/artists [1 5]}}
;   :artists
;   {0 {:artist/name "Bobby Bob" :artist/age 27}
;    1 {:artist/name "Susie Susie" :artist/age 30}
;    2 {:artist/name "Johnny Jon" :artist/age 21}
;    3 {:artist/name "Jimmy Jo" :artist/age 40}
;    4 {:artist/name "Peter Pop" :artist/age 19}
;    5 {:artist/name "Betty Blues" :artist/age 50}}})
;
;(comment
;  (om/tree-pull
;    {:track/name "Cool song"
;     :track/artists [0 2]}
;    [:track/name {:track/artists [:artist/name]}]
;    db #{:track/artists})
;  )
;
;(defui Artist
;  static om/IQuery
;  (-query [this]
;    '[:artist/name :artist/age])
;  Object
;  (render [this]
;    (let [{:keys [:artist/name :artist/age]} (om/props this)]
;      (dom/div nil
;        (dom/div nil
;          (dom/label nil "Artist Name:")
;          (dom/span nil name))
;        (dom/div nil
;          (dom/label nil "Artist Age:")
;          (dom/span nil age))))))
;
;(def artist (om/create-factory Artist))
;
;(defui ArtistList
;  Object
;  (render [this]
;    (apply dom/ul nil
;      (map artist (om/props this)))))
;
;(def artist-list (om/create-factory ArtistList))
;
;(defui Track
;  static om/IQueryParams
;  (-params [this]
;    {:artist (om/query Artist)})
;  static om/IQuery
;  (-query [this]
;    '[:track/name {:track/artists ?artist}])
;  Object
;  (render [this]
;    (let [{:keys [:track/name :track/artists]} (om/props this)]
;      (apply dom/div nil
;        (dom/h2 nil name)
;        (artist-list artists)))))
;
;(def track (om/create-factory Track))
;
;(defui AlbumTracks
;  static om/IQueryParams
;  (-params [this]
;    {:tracks (om/query Track)})
;  static om/IQuery
;  (-query [this]
;    '[:album/name {:album/tracks ?tracks}])
;  Object
;  (render [this]
;    (let [{:keys [:album/name :album/tracks]} (om/props this)]
;      (apply dom/div nil
;        (dom/h1 nil name)
;        (map track tracks)))))
;
;
;(comment
;  (require '[cljs.pprint :as pprint])
;
;  (om/query Artist)
;  (om/query Track)
;  (om/query AlbumTracks)
;
;  (pprint/pprint (om/build-index AlbumTracks))
;  )