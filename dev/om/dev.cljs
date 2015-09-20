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

;; -----------------------------------------------------------------------------
;; Parsing

(defmulti prop (fn [_ k] k))

(defmethod prop :default
  [{:keys [state selector]} k]
  (let [st @state]
    (if (contains? st k)
      {:value (get st k)}
      {:quote (or selector k)})))

(defmulti call (fn [_ k _] k))

(defmethod call :default [_ k _]
  {:quote k})

(defmethod prop :counters/list
  [{:keys [state]} _]
  (let [st @state]
    {:value (into [] (map #(get-in st %)) (:counters/list st))}))

(defmethod call 'counter/increment
  [{:keys [state ref]} _ _]
  (let [{:keys [root id]} ref]
    (swap! state update-in [root id :counter/count] inc))
  {:value []})

;; TODO: doesn't work yet

(defmethod call 'counters/delete
  [{:keys [state ref]} _ _]
  (swap! state update-in [:counters/list] remove #{ref})
  {:value [:counters/list]})

;; TODO: doesn't work yet

(defmethod call 'counters/create
  [{:keys [state]} _ new-todo]
  (swap! state
    (fn [state]
      (let [new-todo (merge new-todo
                       {:db/id (:counters/cur-id state)})]
        (-> state
          (update-in [:counters/list] conj new-todo)
          (update-in [:counters/cur-id] inc)))))
  {:value [:counters/list]})

;; -----------------------------------------------------------------------------
;; Counter

(defui Counter
  static om/IQuery
  (query [this]
    '[:db/id :counter/count])
  Object
  (render [this]
    (let [{:keys [:counter/count] :as props} (om/props this)]
      (dom/div nil
        (dom/p nil (str "Count: " count))
        (dom/button
          #js {:onClick (fn [_] (om/call this 'counter/increment))}
          "Click Me!")
        (dom/button
          #js {:style #js {:marginLeft "10px"}
               :onClick (fn [_] (om/call this 'counters/delete))}
          "Delete")))))

(def counter (om/create-factory Counter))

;; -----------------------------------------------------------------------------
;; HelloWorldTitle

(defui HelloWorldTitle
  Object
  (render [this]
    (apply dom/div nil
      (om/children this))))

(def app-title (om/create-factory HelloWorldTitle))

;; -----------------------------------------------------------------------------
;; HelloWorld

(defui HelloWorld
  static om/IQueryParams
  (params [this]
    {:counter (om/get-query Counter)})
  static om/IQuery
  (query [this]
    '[:app/title {:counters/list ?counter}])
  Object
  (render [this]
    (let [{:keys [:app/title :counters/list] :as props}
          (om/props this)]
      (apply dom/div nil
        (app-title nil
          (dom/h2 nil "Hello World!")
          (dom/h3 nil "cool stuff"))
        (dom/div nil
          (dom/button
            #js {:onClick (fn [e] (om/call this 'counters/add!))}
            "Add Counter!"))
        (map-indexed
          (fn [i props]
            (counter (assoc props :om-index i)))
          list)))))

;; -----------------------------------------------------------------------------
;; Reconciler setup

(def app-state
  (atom {:app/title "Hello World!"
         :app/current-id 3
         :app/counters
         {0 {:db/id 0 :counter/count 0}
          1 {:db/id 1 :counter/count 0}
          2 {:db/id 2 :counter/count 0}}
         :counters/list (om/refs :app/counters 0 1 2)}))

(defmulti ui->ref om/react-type)

(defmethod ui->ref :default [c] nil)

(defmethod ui->ref Counter
  [c] (om/ref :app/counters (:db/id (om/props c))))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser (om/parser {:read prop :mutate call})
     :ui->ref ui->ref}))

(om/add-root! reconciler
  (gdom/getElement "app") HelloWorld)

(comment
  (ui->ref (counter {:db/id 0 :counter/count 0}))

  (require '[cljs.pprint :as pprint])
  (pprint/pprint (om/build-index HelloWorld))

  (-> (om/build-index HelloWorld)
    :prop->component :id)

  ((om/parser {:read prop :mutate call})
    {:state app-state}
    (om/get-query HelloWorld))
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