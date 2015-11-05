(ns om.devcards.core
  (:require-macros [devcards.core :refer [defcard deftest]])
  (:require [cljs.test :refer-macros [is async]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(defui Hello
  Object
  (render [this]
    (dom/p nil (-> this om/props :text))))

(def hello (om/factory Hello))

(defcard simple-component
  "Test that Om Next component work as regular React components."
  (hello {:text "Hello, world!"}))

(def p
  (om/parser
    {:read   (fn [_ _ _] {:remote true})
     :mutate (fn [_ _ _] {:remote true})}))

(def r
  (om/reconciler
    {:parser p
     :ui->ref (fn [c] (-> c om/props :id))}))

(defui Binder
  Object
  (componentDidMount [this]
    (let [indexes @(get-in (-> this om/props :reconciler) [:config :indexer])]
      (om/update-state! this assoc :indexes indexes)))
  (render [this]
    (binding [om/*reconciler* (-> this om/props :reconciler)]
      (apply dom/div nil
        (hello {:id 0 :text "Goodbye, world!"})
        (when-let [indexes (get-in (om/get-state this)
                             [:indexes :ref->components])]
          [(dom/p nil (pr-str indexes))])))))

(def binder (om/factory Binder))

(defcard basic-nested-component
  "Test that component nesting works"
  (binder {:reconciler r}))

(deftest test-indexer
  "Test indexer"
  (let [idxr (get-in r [:config :indexer])]
    (is (not (nil? idxr)) "Indexer is not nil in the reconciler")
    (is (not (nil? @idxr)) "Indexer is IDeref")))

;; -----------------------------------------------------------------------------
;; Counteres

(defmulti counters-read (fn [_ k] k))

(defmulti counters-mutate (fn [_ k _] k))

(defmethod counters-read :default
  [{:keys [state]} k params]
  (let [st @state]
    (if (contains? st k)
      {:value (get st k)}
      {:remote true})))

(defmethod counters-read :counters/list
  [{:keys [state selector]} _]
  (let [st @state
        xf (map #(select-keys (get-in st %) selector))]
    {:value (into [] xf (:counters/list st))}))

(defmethod counters-mutate 'counter/increment
  [{:keys [state ref]} _ _]
  {:value []
   :action
   (fn []
     (swap! state update-in (conj ref :counter/count) inc))})

(defmethod counters-mutate 'counters/delete
  [{:keys [state ref]} _ _]
  {:value [:counters/list]
   :action
   (fn []
     (swap! state
       (fn [state]
         (-> state
           (update-in (pop ref) dissoc (peek ref))
           (update-in [:counters/list] #(vec (remove #{ref} %)))))))})

(defmethod counters-mutate 'counters/create
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
             (update-in [:counters/list] conj [:app/counters id])
             (update-in [:app/current-id] inc))))))})

;; -----------------------------------------------------------------------------
;; Counter

(defui Counter
  static om/IQuery
  (query [this]
    '[:id :counter/count])
  om/Ident
  (ident [this {:keys [id]}]
    [:app/counters id])
  Object
  (initLocalState [this]
    {:state-count 0})
  (componentWillUnmount [this]
    (println "Buh-bye!"))
  (componentWillUpdate [this next-props next-state]
    (println "component will update" (om/props this) next-props))
  (componentDidUpdate [this prev-props prev-state]
    #_(println "component did update" (om/props this) prev-props))
  (render [this]
    (let [{:keys [:counter/count] :as props} (om/props this)]
      (dom/div nil
        (dom/p nil
          (str "Count: " count
            " State Count: " (om/get-state this :state-count)))
        (dom/button
          #js {:onClick
               (fn [_] (om/transact! this '[(counter/increment)]))}
          "Update Props, Click Me!")
        (dom/button
          #js {:style #js {:marginLeft "10px"}
               :onClick
               (fn [_] (om/update-state! this update-in [:state-count] inc))}
          "Update State, Click Me!")
        (dom/button
          #js {:style #js {:marginLeft "10px"}
               :onClick
               (fn [_] (om/transact! this '[(counters/delete) :counters/list]))}
          "Delete")))))

(def counter (om/factory Counter {:keyfn :id}))

;; -----------------------------------------------------------------------------
;; CountersAppTitle

(defui CountersAppTitle
  Object
  (render [this]
    (apply dom/div nil
      (om/children this))))

(def counters-app-title (om/factory CountersAppTitle))

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
        (counters-app-title nil
          (dom/h2 #js {:key "a"} "Hello World!")
          (dom/h3 #js {:key "b"} "cool stuff"))
        (dom/div nil
          (dom/button
            #js {:onClick (fn [e] (om/transact! this '[(counters/create)]))}
            "Add Counter!"))
        (map counter list)))))

;; -----------------------------------------------------------------------------
;; Reconciler setup

(def counters-app-state
  (atom {:app/title "Hello World!"
         :app/current-id 3
         :app/counters
         {0 {:id 0 :counter/count 0}
          1 {:id 1 :counter/count 0}
          2 {:id 2 :counter/count 0}}
         :counters/list [[:app/counters 0]
                         [:app/counters 1]
                         [:app/counters 2]]}))

(def counters-reconciler
  (om/reconciler
    {:state  counters-app-state
     :parser (om/parser {:read counters-read :mutate counters-mutate})}))

(defcard test-counters
  "Test that we can mock a reconciler backed Om Next component into devcards"
  (om/mock-root counters-reconciler CountersApp))