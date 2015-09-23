(ns om.dev
  (:refer-clojure :exclude [var?])
  (:require [clojure.browser.repl :as repl]
            [om.next :as om :refer-macros [defui]]
            [om.next.protocols :as p]
            [om.dom :as dom]
            [goog.object :as gobj]
            [goog.dom :as gdom]
            [cljs.pprint :as pprint]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

;; -----------------------------------------------------------------------------
;; Parsing

(defmulti read (fn [_ k] k))

(defmulti mutate (fn [_ k _] k))

(defmethod read :default
  [{:keys [state]} k params]
  (let [st @state]
    (if (contains? st k)
     {:value (get st k)}
     {:quote true})))

(defmethod read :counters/list
  [{:keys [state selector]} _]
  (let [st @state
        xf (map #(select-keys (get-in st %) selector))]
    {:value (into [] xf (:counters/list st))}))

(defmethod mutate 'counter/increment
  [{:keys [state ref]} _ _]
  {:value []
   :action
   (fn []
     (swap! state update-in (conj ref :counter/count) inc))})

(defmethod mutate 'counters/delete
  [{:keys [state ref]} _ _]
  {:value [:counters/list]
   :action
   (fn []
     (swap! state
       (fn [state]
         (-> state
           (update-in (pop ref) dissoc (peek ref))
           (update-in [:counters/list] #(vec (remove #{ref} %)))))))})

(defmethod mutate 'counters/create
  [{:keys [state]} _ _]
  {:value [:counters/list]
   :action
   (fn []
     (swap! state
       (fn [state]
         (let [id (:app/current-id state)
               counter {:id id :counter/count 0}]
           (-> state
             (assoc-in [:app/counters id] counter)
             (update-in [:counters/list] conj (om/ref :app/counters id))
             (update-in [:app/current-id] inc))))))})

;; -----------------------------------------------------------------------------
;; Counter

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
          #js {:onClick (fn [_] (om/call this 'counter/increment))}
          "Click Me!")
        (dom/button
          #js {:style #js {:marginLeft "10px"}
               :onClick (fn [_] (om/call this 'counters/delete))}
          "Delete")))))

(def counter (om/create-factory Counter))

;; -----------------------------------------------------------------------------
;; CountersAppTitle

(defui CountersAppTitle
  Object
  (render [this]
    (apply dom/div nil
      (om/children this))))

(def app-title (om/create-factory CountersAppTitle))

;; -----------------------------------------------------------------------------
;; CountersApp

(defui CountersApp
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
            #js {:onClick (fn [e] (om/call this 'counters/create))}
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
         {0 {:id 0 :counter/count 0}
          1 {:id 1 :counter/count 0}
          2 {:id 2 :counter/count 0}}
         :counters/list (om/refs :app/counters 0 1 2)}))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser (om/parser {:read read :mutate mutate})
     :ui->ref (fn [c]
                (if (instance? Counter c)
                  (om/ref :app/counters (-> c om/props :id))
                  c))}))

(om/add-root! reconciler
  (gdom/getElement "app") CountersApp)

(comment
  (require '[cljs.pprint :as pprint])

  (pprint/pprint (om/build-index CountersApp))

  (def idxr (get-in reconciler [:config :indexer]))

  (pprint/pprint @(:indexes idxr))

  (def c
    (first (get-in @(:indexes idxr)
             [:ref->components (om/ref :app/counters 1)])))

  ;; works
  (def dp (om/data-path c))

  (def cp (om/class-path c))

  (def q (get-in @(:indexes idxr) [:class-path->query cp]))

  ;; TODO: doesn't work
  (om/state-path q dp)
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