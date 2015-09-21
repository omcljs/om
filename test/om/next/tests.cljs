(ns om.next.tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [om.next :as om :refer-macros [defui]]
            [om.next.protocols :as p]
            [om.dom :as dom]))

;; -----------------------------------------------------------------------------
;; Components

(defui Component
  static om/IQuery
  (query [this]
    '[:foo/bar :baz/woz])
  Object
  (render [this]))

(defui ComponentList
  static om/IQueryParams
  (params [this]
    {:component (om/get-query Component)})
  static om/IQuery
  (query [this]
    '[{:components/list ?component} :app/title])
  Object
  (render [this]))

(deftest test-component?
  (is (om/component? (Component. {}))))

(deftest test-pr-str-component
  (is (= (pr-str Component) "om.next.tests/Component")))

;; -----------------------------------------------------------------------------
;; Queries

(deftest test-query
  (is (= (om/query Component)
         '[:foo/bar :baz/woz]))
  (is (= (om/query ComponentList)
         '[{:components/list ?component} :app/title])))

(deftest test-get-query
  (is (= (om/get-query Component)
         '[:foo/bar :baz/woz]))
  (is (= (om/get-query ComponentList)
         '[{:components/list [:foo/bar :baz/woz]} :app/title])))

(deftest test-focus-selector
  (is (= (om/focus-query [:foo/bar] [])
         [:foo/bar]))
  (is (= (om/focus-query
           [:foo/bar {:baz/woz [:goz/noz]}]
           [:baz/woz])
         [{:baz/woz [:goz/noz]}]))
  (is (= (om/focus-query
           [:foo/bar {:baz/woz [:goz/noz {:bop/wop [:nop/sop]} :cuz/wuz]}]
           [:baz/woz :bop/wop])
        [{:baz/woz [{:bop/wop [:nop/sop]}]}])))

(deftest test-focus->path
  (is (= (om/focus->path [{:baz/woz [{:bop/wop [:nop/sop]}]}])
         [:baz/woz :bop/wop]))
  (is (= (om/focus->path [:app/title {:counters/list [:db/id :counter/count]}])
         [])))

(deftest test-state-path
  (is (= (om/state-path [{:todos/list [:title]}] '(1))
         '[({:todos/list [:title]} {:index 1})])))

;; -----------------------------------------------------------------------------
;; Indexer

(deftest test-indexer
  (let [idxr (om/indexer identity)
        idxs (p/index-root idxr ComponentList)]
    (is (= (set (keys (:prop->classes idxs)))
           #{:app/title :components/list :foo/bar :baz/woz}))
    (is (= (get-in idxs [:class->paths Component])
           #{[:components/list]}))
    (is (= (get-in idxs [:class->selectors Component])
           #{[{:components/list [:foo/bar :baz/woz]}]}))))

(deftest test-reconciler-has-indexer
  (let [r (om/reconciler
            {:state (atom nil)
             :ui->ref identity})]
    (is (instance? om/Indexer (get-in r [:config :indexer])))))

;; -----------------------------------------------------------------------------
;; Refs

(deftest test-ref
  (let [r  (om/ref :foo 0)
        rs (om/refs :foo 0)]
    (is (= :foo (:root r)))
    (is (= 0 (:id r)))
    (is (= :foo (:root (first rs))))
    (is (= 0 (:id (first rs))))
    (is (= (om/ref :foo 0) (om/ref :foo 0)))
    (is (= (seq (om/ref :foo 0)) [:foo 0]))
    (is (= (seq (conj (om/ref :foo 0) :bar)) [:foo 0 :bar]))
    (is (= (get-in {:foo {:bar {:baz 1}}} (om/ref :foo :bar :baz)) 1))))

;; -----------------------------------------------------------------------------
;; Parser

(defmulti read (fn [env k params] k))

(defmethod read :default
  [{:keys [state data]} k params]
  (if (and (not (nil? data)) (contains? data k))
    {:value (get data k)}
    {:quote true}))

(defmethod read :foo/bar
  [{:keys [state]} k params]
  (if-let [v (get @state k)]
    {:value v}
    {:quote true}))

(defmethod read :woz/noz
  [{:keys [state]} k params]
  (if-let [v (get @state k)]
    {:value v :quote true} ;; local read AND remote read
    {:quote true})) ;; no cached locally, must read remote

(defmethod read :user/pic
  [env k {:keys [size]}]
  (let [size-str (case size :small "50x50" :large "100x100")]
    {:value (str "user" size-str ".png") :quote true}))

(defmulti mutate (fn [env k params] k))

(defmethod mutate 'do/it!
  [{:keys [state]} k {:keys [id]}]
  {:value [id] :quote true})

(def p (om/parser {:read read :mutate mutate}))

(deftest test-basic-parsing
  (let [st (atom {:foo/bar 1})]
    (is (= (p {} [:baz/woz]) {}))
    (is (= (p {:state st} [:foo/bar]) {:foo/bar 1}))
    (is (= (p {:state st} [:foo/bar :baz/woz]) {:foo/bar 1}))
    (is (= (p {} [:baz/woz] true) [:baz/woz]))
    (is (= (p {:state st} [:foo/bar] true) []))
    (is (= (p {:state st} [:foo/bar :baz/woz] true) [:baz/woz]))))

(deftest test-value-and-quote
  (let [st (atom {:woz/noz 1})]
    (is (= (p {:state st} [:woz/noz]) {:woz/noz 1}))
    (is (= (p {:state st} [:woz/noz] true) [:woz/noz]))))

(deftest test-call
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[(do/it! {:id 0})]) '{do/it! [0]}))
    (is (= (p {} '[(do/it! {:id 0})] true)
           '[(do/it! {:id 0})]))))

(deftest test-read-call
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[(:user/pic {:size :small})])
           {:user/pic "user50x50.png"}))
    (is (= (p {:state st} '[(:user/pic {:size :small})] true)
           '[(:user/pic {:size :small})]))))

(defmethod mutate 'mutate!
  [{:keys [state]} k params]
  (swap! state update-in [:count] inc))

(deftest test-quote-does-not-mutate
  (let [st (atom {:count 0})
        _  (p {:state st} '[(mutate!)])
        _  (p (:state st) '[(mutate!)] true)]
    (is (= @st {:count 1}))))

(defmethod read :now/wow
  [{:keys [state selector]} k params]
  {:value {:selector selector :params params}})

(deftest test-parameterized-ref
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[({:now/wow [:a :b]} {:slice [10 20]})])
           '{:now/wow {:selector [:a :b] :params {:slice [10 20]}}}))))

;; -----------------------------------------------------------------------------
;; Recursive Parsing

(def todos-state
  (atom
    {:todos
     {0 {:id 0
         :title "Walk dog"
         :completed false
         :category 0}
      1 {:id 0
         :title "Get milk"
         :completed true
         :category 0}
      2 {:id 0
         :title "Finish Om Next"
         :completed false
         :category 1}}
     :categories {0 :home 1 :work}
     :todos/list [0 1 2]}))

(defmethod read :category
  [{:keys [state data]} k]
  {:value (get-in @state [:categories (get data k)])})

(defmethod read :todos/list
  [{:keys [state selector parse] :as env} _]
  (let [st @state
        pf #(parse (assoc env :data %) selector)]
    {:value (into [] (comp (map (:todos st)) (map pf))
              (:todos/list st))}))

(deftest test-recursive-parse
  (is (= (p {:state todos-state} '[{:todos/list [:title :category]}])
         '{:todos/list [{:title "Walk dog", :category :home}
                        {:title "Get milk", :category :home}
                        {:title "Finish Om Next", :category :work}]})))

(comment
  (run-tests)

  (require '[cljs.pprint :as pp])

  (p {:state todos-state} '[{:todos/list [:title :category]}])

  (let [idxr (om/indexer identity)]
    (pp/pprint (p/index-root idxr ComponentList)))
  )