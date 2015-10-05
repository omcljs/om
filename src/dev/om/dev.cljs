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
     {:remote true})))

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
          #js {:onClick (fn [_] (om/transact! this '[(counter/increment)]))}
          "Click Me!")
        (dom/button
          #js {:style #js {:marginLeft "10px"}
               :onClick
               (fn [_] (om/transact! this '[(counters/delete) :counters/list]))}
          "Delete")))))

(def counter (om/factory Counter))

;; -----------------------------------------------------------------------------
;; CountersAppTitle

(defui CountersAppTitle
  Object
  (render [this]
    (apply dom/div nil
      (om/children this))))

(def app-title (om/factory CountersAppTitle))

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
            #js {:onClick (fn [e] (om/transact this '[(counters/create)]))}
            "Add Counter!"))
        (map-indexed
          (fn [i props]
            (counter (assoc props :react-key (:id props) :om-index i)))
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

(om/add-root! reconciler CountersApp (gdom/getElement "app"))

(comment
  (require '[cljs.pprint :as pprint])

  (pprint/pprint @app-state)

  (pprint/pprint (om/build-index CountersApp))

  (def idxr (get-in reconciler [:config :indexer]))

  (p/key->components idxr :counters/list)

  (pprint/pprint @(:indexes idxr))

  (def c
    (first (get-in @(:indexes idxr)
             [:ref->components (om/ref :app/counters 1)])))

  ;; works
  (def dp (om/data-path c))

  (def cp (om/class-path c))

  (def q (get-in @(:indexes idxr) [:class-path->query cp]))

  (om/state-path idxr c)
  (om/key->paths idxr (om/ref :app/counters 0))
  )
