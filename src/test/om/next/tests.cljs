(ns om.next.tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [goog.object :as gobj]
            [clojure.zip :as zip]
            [om.next :as om :refer-macros [defui]]
            [om.next.protocols :as p]
            [om.next.impl.parser :as parser]
            [om.dom :as dom]))

;; -----------------------------------------------------------------------------
;; Components

(defui ^:once Component
  static om/IQuery
  (query [this]
    '[:foo/bar :baz/woz]))

(def component (om/factory Component))

(defui ComponentList
  static om/IQueryParams
  (params [this]
    {:component (om/get-query Component)})
  static om/IQuery
  (query [this]
    '[{:components/list ?component} :app/title]))

(def component-list (om/factory ComponentList))

(deftest test-component?
  (is (om/component? (Component. {}))))

(deftest test-pr-str-component
  (is (= (pr-str Component) "om.next.tests/Component")))

(deftest test-construction
  (let [c (component-list {:foo/bar 1})]
    (is (= (om/react-type c) ComponentList))
    (is (= (om/t c) 0))))

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

(comment
  (om/focus-query '[{:tree [:id :node-value {:children ...}]}]
    [:tree :children])
  )

(deftest test-om-423
  (is (= (om/focus-query '[:foo ({:people [:name :age]} {:length 3}) :bar] [:people])
         '[({:people [:name :age]} {:length 3})])))

(deftest test-focus-query-union
  (is (= (om/focus-query [{:selected/item {:item/one [:title]
                                           :item/two [:author]}}]
                         [:selected/item :item/two])
         [{:selected/item {:item/two [:author] :om.next/union true}}])))

;; -----------------------------------------------------------------------------
;; Query Templating

(deftest test-node->key
  (is (= :foo (om/node->key {:foo []})))
  (is (= :foo (om/node->key '({:foo []} {:bar :baz})))))

(deftest test-query-template
  (is (= [:app/title {:todos/list [:db/id :todo/title]}]
         (-> (om/query-template
               [:app/title {:todos/list [:db/id :todo/title :todo/completed]}]
               [:todos/list])
           (om/replace [:db/id :todo/title]))))
  (is (= '[:app/title ({:todos/list [:db/id :todo/title]} {:start 0 :end 10})]
         (-> (om/query-template
               '[:app/title ({:todos/list [:db/id :todo/title :todo/completed]}
                             {:start 0 :end 10})]
               '[:todos/list])
           (om/replace [:db/id :todo/title])))))

(deftest test-query-template-root
  (is (= [:app/title]
         (-> (om/query-template
               [:app/title {:todos/list [:db/id :todo/title :todo/completed]}]
               [])
           (om/replace [:app/title])))))

(deftest test-query-template-union
  (is (= (-> (om/query-template
               [{:selected/item {:item/one [:title]
                                 :item/two [:author]}}]
               [:selected/item :item/two])
           zip/root)
         [{:selected/item [:author]}])
      (= (-> (om/query-template
               [{:selected/item {:item/one [:title]
                                 :item/two [:author]}}]
               [:selected/item :item/two])
           (om/replace [:caption]))
        [{:selected/item [:caption]}])))

;; -----------------------------------------------------------------------------
;; Indexer

(deftest test-indexer
  (let [idxr (om/indexer)
        idxs (p/index-root idxr ComponentList)]
    (is (= (set (keys (:prop->classes idxs)))
           #{:app/title :components/list :foo/bar :baz/woz}))
    ))

(deftest test-reconciler-has-indexer
  (let [r (om/reconciler
            {:state (atom nil)
             :ui->ref identity})]
    (is (instance? om/Indexer (get-in r [:config :indexer])))))

;; -----------------------------------------------------------------------------
;; Parser

(deftest test-expr->ast
  (is (= (parser/expr->ast :foo)
         {:type :prop :key :foo :dkey :foo}))
  (is (= (parser/expr->ast [:foo 0])
         {:type :prop :key [:foo 0] :dkey :foo :params {:om.next/refid 0}}))
  (is (= (parser/expr->ast {:foo [:bar]})
         {:type :prop :key :foo :dkey :foo :sel [:bar]}))
  (is (= (parser/expr->ast {[:foo 0] [:bar]})
          {:type :prop :key [:foo 0] :dkey :foo :params {:om.next/refid 0} :sel [:bar]}))
  (is (= (parser/expr->ast '(:foo {:bar 1}))
         {:type :prop :key :foo :dkey :foo :params {:bar 1}}))
  (is (= (parser/expr->ast '({:foo [:bar :baz]} {:woz 1}))
         {:type :prop :key :foo :dkey :foo :sel [:bar :baz] :params {:woz 1}}))
  (is (= (parser/expr->ast '({[:foo 0] [:bar :baz]} {:woz 1}))
         {:type :prop :key [:foo 0] :dkey :foo :sel [:bar :baz] :params {:om.next/refid 0 :woz 1}}))
  (is (= (parser/expr->ast '(do/it {:woz 1}))
         {:type :call :key 'do/it :dkey 'do/it :params {:woz 1}}))
  (is (= (parser/expr->ast '(do/it))
         {:type :call :key 'do/it :dkey 'do/it :params {}})))

(deftest test-ast->expr
  (is (= (parser/ast->expr {:type :prop :key :foo :dkey :foo})
         :foo))
  (is (= (parser/ast->expr {:type :prop :key [:foo 0] :dkey :foo :params {:om.next/refid 0}})
         [:foo 0]))
  (is (= (parser/ast->expr {:type :prop :key :foo :dkey :foo :sel [:bar]})
         {:foo [:bar]}))
  (is (= (parser/ast->expr {:type :prop :key [:foo 0] :dkey :foo :params {:om.next/refid 0} :sel [:bar]})
         {[:foo 0] [:bar]}))
  (is (= (parser/ast->expr {:type :prop :key :foo :dkey :foo :params {:bar 1}})
         '(:foo {:bar 1})))
  (is (= (parser/ast->expr {:type :prop :key :foo :dkey :foo :sel [:bar :baz] :params {:woz 1}})
         '({:foo [:bar :baz]} {:woz 1})))
  (is (= (parser/ast->expr {:type :prop :key [:foo 0] :dkey :foo :sel [:bar :baz] :params {:om.next/refid 0 :woz 1}})
         '({[:foo 0] [:bar :baz]} {:woz 1})))
  (is (= (parser/ast->expr {:type :call :key 'do/it :dkey 'do/it :params {:woz 1}})
         '(do/it {:woz 1})))
  (is (= (parser/ast->expr {:type :call :key 'do/it :dkey 'do/it :params {}})
         '(do/it))))

(defmulti read (fn [env k params] k))

(defmethod read :default
  [{:keys [state data]} k params]
  (if (and (not (nil? data)) (contains? data k))
    {:value (get data k)}
    {:remote true}))

(defmethod read :foo/bar
  [{:keys [state]} k params]
  (if-let [v (get @state k)]
    {:value v}
    {:remote true}))

(defmethod read :woz/noz
  [{:keys [state]} k params]
  (if-let [v (get @state k)]
    {:value v :remote true} ;; local read AND remote read
    {:remote true})) ;; no cached locally, must read remote

(defmethod read :user/pic
  [env k {:keys [size]}]
  (let [size-str (case size :small "50x50" :large "100x100")]
    {:value (str "user" size-str ".png") :remote true}))

(defmethod read :user/by-id
  [{:keys [selector] :as env} k {:keys [id] :as params}]
  {:value (cond-> {:name/first "Bob" :name/last "Smith"}
            selector (select-keys selector))
   :remote true})

(defmulti mutate (fn [env k params] k))

(defmethod mutate 'do/it!
  [{:keys [state]} k {:keys [id]}]
  {:value [id]
   :action #()
   :remote true})

(def p (om/parser {:read read :mutate mutate}))

(deftest test-basic-parsing
  (let [st (atom {:foo/bar 1})]
    (is (= (p {} [:baz/woz]) {}))
    (is (= (p {:state st} [:foo/bar]) {:foo/bar 1}))
    (is (= (p {:state st} [:foo/bar :baz/woz]) {:foo/bar 1}))
    (is (= (p {} [:baz/woz] :remote) [:baz/woz]))
    (is (= (p {:state st} [:foo/bar] :remote) []))
    (is (= (p {:state st} [:foo/bar :baz/woz] :remote) [:baz/woz]))))

(deftest test-value-and-remote
  (let [st (atom {:woz/noz 1})]
    (is (= (p {:state st} [:woz/noz]) {:woz/noz 1}))
    (is (= (p {:state st} [:woz/noz] :remote) [:woz/noz]))))

(deftest test-call
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[(do/it! {:id 0})]) '{do/it! [0]}))
    (is (= (p {} '[(do/it! {:id 0})] :remote)
           '[(do/it! {:id 0})]))))

(deftest test-read-call
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[(:user/pic {:size :small})])
           {:user/pic "user50x50.png"}))
    (is (= (p {:state st} '[(:user/pic {:size :small})] :remote)
           '[(:user/pic {:size :small})]))))

(defmethod mutate 'mutate!
  [{:keys [state]} k params]
  {:value  []
   :action #(swap! state update-in [:count] inc)} )

(deftest test-remote-does-not-mutate
  (let [st (atom {:count 0})
        _  (p {:state st} '[(mutate!)])
        _  (p (:state st) '[(mutate!)] :remote)]
    (is (= @st {:count 1}))))

(defmethod read :now/wow
  [{:keys [state selector]} k params]
  {:value {:selector selector :params params}})

(deftest test-parameterized-join
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[({:now/wow [:a :b]} {:slice [10 20]})])
           '{:now/wow {:selector [:a :b] :params {:slice [10 20]}}}))))

(deftest test-refs
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} [[:user/by-id 0]])
           {[:user/by-id 0] {:name/first "Bob" :name/last "Smith"}}))
    (is (= (p {:state st} [[:user/by-id 0]] :remote)
           [[:user/by-id 0]]))
    (is (= (p {:state st} [{[:user/by-id 0] [:name/last]}])
           {[:user/by-id 0] {:name/last "Smith"}}))
    (is (= (p {:state st} [{[:user/by-id 0] [:name/last]}] :remote)
           [{[:user/by-id 0] [:name/last]}]))))

(deftest test-forced-remote
  (is (= (p {} '['(foo/bar)]) {}))
  (is (= (p {} '['(foo/bar)] :remote) '[(foo/bar)])))

(defmethod mutate 'this/throws
  [_ _ _]
  {:action #(throw (js/Error.))})

(deftest test-throw
  (is (instance? js/Error (get (p {} '[(this/throws)]) 'this/throws))))

;; -----------------------------------------------------------------------------
;; Edge cases

(defmethod read :missing/thing
  [env k params]
  {})

(deftest test-missing-value
  (is (= (p {} [:missing/thing]) {})))

(defmethod mutate 'remote/action
  [env k params] {:remote true})

(defmethod mutate 'action/no-value
  [{:keys [state] :as env} k params]
  {:action (fn [] (reset! state :changed))})

(deftest test-action-no-value
  (let [state (atom nil)]
    (is (= (p {:state state} '[(action/no-value)]) {}))
    (is (= :changed @state)))
  (let [state (atom nil)]
    (is (= (p {:state state} '[(action/no-value)] :remote) []))
    (is (= nil @state))))

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
  [{:keys [state selector parser] :as env} _]
  (let [st @state
        pf #(parser (assoc env :data %) selector)]
    {:value (into [] (comp (map (:todos st)) (map pf))
              (:todos/list st))}))

(deftest test-recursive-parse
  (is (= (p {:state todos-state} '[{:todos/list [:title :category]}])
         '{:todos/list [{:title "Walk dog", :category :home}
                        {:title "Get milk", :category :home}
                        {:title "Finish Om Next", :category :work}]})))

;; -----------------------------------------------------------------------------
;; Normalization

(def data
  {:list/one [{:name "John" :points 0 :friend {:name "Bob"}}
              {:name "Mary" :points 0 :foo :bar}
              {:name "Bob" :points 0 :friend {:name "John"}}]
   :list/two [{:name "Gwen" :points 0 :friends [{:name "Jeff"}]}
              {:name "Mary" :points 0 :baz :woz}
              {:name "Jeff" :points 0 :friends [{:name "Gwen"}]}]})

(defui Person
  static om/Ident
  (ident [this {:keys [name]}]
    [:person/by-name name])
  static om/IQuery
  (query [this]
    [:name :points
     {:friend (om/tag [:name] Person)}
     {:friends (om/tag [:name] Person)}
     :foo :baz])
  Object
  (render [this]))

(defui ListView
  Object
  (render [this]))

(defui RootView
  static om/IQuery
  (query [this]
    (let [subquery (om/get-query Person)]
      [{:list/one subquery} {:list/two subquery}]))
  Object
  (render [this]))

(deftest test-tree->db
  (let [norm (om/tree->db RootView data)
        refs (meta norm)
        p0   (get-in refs [:person/by-name "Mary"])]
    (is (= 3 (count (get norm :list/one))))
    (is (= {:name "John" :points 0 :friend [:person/by-name "Bob"]}
           (get-in refs [:person/by-name "John"])))
    (is (= 3 (count (get norm :list/two))))
    (is (contains? p0 :foo))
    (is (contains? p0 :baz))))

(deftest test-incremental-tree->db
  (let [p0   (om/tree->db Person
               {:name "Susan" :points 5 :friend {:name "Mary"}})
        refs (meta p0)]
    (is (= {:name "Susan" :points 5 :friend [:person/by-name "Mary"]}
           p0))
    (is (= refs {:person/by-name {"Mary" {:name "Mary"}}}))))

(comment
  (require '[cljs.pprint :as pp])
  (run-tests)
  )

;; -----------------------------------------------------------------------------
;; Denormalization

(deftest test-db->tree
  (let [orig {:name "Susan" :points 5 :friend {:name "Mary"} :friends []}
        p0   (om/tree->db Person orig)
        refs (meta p0)]
    (is (= orig (om/db->tree (om/get-query Person) p0 refs)))))

(def people-data
  {:people [{:id 0 :name "Bob" :friends []}
            {:id 1 :name "Laura" :friends []}
            {:id 2 :name "Mary" :friends []}]})

(defui Friend1
  static om/Ident
  (ident [this props]
    [:person/by-id (:id props)])
  static om/IQuery
  (query [this]
    [:id :name]))

(defui Person1
  static om/Ident
  (ident [this props]
    [:person/by-id (:id props)])
  static om/IQuery
  (query [this]
    [:id :name {:friends (om/get-query Friend1)}]))

(defui People1
  static om/IQuery
  (query [this]
    [{:people (om/get-query Person1)}]))

(defmulti read2 om/dispatch)

(defmethod read2 :people
  [{:keys [state selector] :as env} key _]
  (let [st @state]
    {:value (om/db->tree selector (get st key) st)}))

(defn add-friend [state id friend]
  (if (not= id friend)
    (letfn [(add* [friends ref]
              (cond-> friends
                (not (some #{ref} friends)) (conj ref)))]
      (-> state
        (update-in [:person/by-id id :friends]
          add* [:person/by-id friend])
        (update-in [:person/by-id friend :friends]
          add* [:person/by-id id])))
    state))

(deftest test-db->tree-collection
  (let [norm-data  (om/tree->db People1 people-data true)
        app-state  (atom norm-data)
        parser     (om/parser {:read read2})
        norm-data' (add-friend (om/tree->db People1 people-data true) 0 1)
        app-state' (atom norm-data')]
    (is (= (parser {:state app-state} (om/get-query People1))
           {:people [{:id 0, :name "Bob", :friends []}
                     {:id 1, :name "Laura", :friends []}
                     {:id 2, :name "Mary", :friends []}]}))
    (is (= (parser {:state app-state'} (om/get-query People1))
          {:people [{:id 0, :name "Bob", :friends [{:id 1, :name "Laura"}]}
                    {:id 1, :name "Laura", :friends [{:id 0, :name "Bob"}]}
                    {:id 2, :name "Mary", :friends []}]}))))

;; -----------------------------------------------------------------------------
;; Message Forwarding

(defui Post
  static om/IQuery
  (query [this]
    [:id :type :title :author :content]))

(defui Photo
  static om/IQuery
  (query [this]
    [:id :type :title :image :caption]))

(defui Graphic
  static om/IQuery
  (query [this]
    [:id :type :title :image]))

(defui DashboardItem
  static om/Ident
  (ident [this {:keys [id type]}]
    [type id])
  static om/IQuery
  (query [this]
    (zipmap
      [:dashboard/post :dashboard/photo :dashboard/graphic]
      (map #(conj % :favorites)
        [(om/get-query Post)
         (om/get-query Photo)
         (om/get-query Graphic)]))))

(defui Dashboard
  static om/IQuery
  (query [this]
    [{:dashboard/items (om/get-query DashboardItem)}]))

(defmulti read1 om/dispatch)

(defmethod read1 :default
  [_ _ _])

(defmethod read1 :dashboard/items
  [{:keys [parse ast] :as env} _ _]
  {:remote (update-in ast [:sel]
             #(into {} (map (fn [[k v]] [k [:favorites]])) %))})

(deftest test-recursive-remote
  (let [parser (om/parser {:read read1})]
    (is (= (parser {} (om/get-query Dashboard) :remote)
           [{:dashboard/items
             {:dashboard/post    [:favorites],
              :dashboard/photo   [:favorites],
              :dashboard/graphic [:favorites]}}]))))

;; -----------------------------------------------------------------------------
;; Recursive Components

(def tree-data
  {:tree {:id 0
          :node-value 1
          :children [{:id 1
                      :node-value 2
                      :children [{:id 2
                                  :node-value 3
                                  :children []}]}
                     {:id 3
                      :node-value 4
                      :children []}]}})

(defui Node
  static om/Ident
  (ident [this {:keys [id]}]
    [:node/by-id id])
  static om/IQuery
  (query [this]
    '[:id :node-value {:children ...}]))

(defui Tree
  static om/IQuery
  (query [this]
    [{:tree (om/get-query Node)}]))

(defmulti tree-read om/dispatch)

(defmethod tree-read :default
  [{:keys [data] :as env} k _]
  {:value (get data k)})

(defmethod tree-read :children
  [{:keys [data parser selector] :as env} _ _]
  {:value (let [f #(parser (assoc env :data %) selector)]
            (into [] (map f (:children data))))})

(defmethod tree-read :tree
  [{:keys [state parser selector] :as env} k _]
  (let [st @state]
    {:value (parser (assoc env :data (:tree st)) selector)}))

(deftest test-recursion-syntax
  (let [tree-parser (om/parser {:read tree-read})]
    (is (= tree-data
           (tree-parser {:state (atom tree-data)}
             (om/get-query Tree))))))

(deftest test-normalize-recursive
  (let [db (om/tree->db Tree tree-data true)]
    (is (= [:node/by-id 0] (:tree db)))
    (is (contains? db :node/by-id))
    (is (= tree-data
           (om/db->tree (om/get-query Tree)
             (om/tree->db Tree tree-data)
             (om/tree->db Tree tree-data true))))))

(deftest test-normalize-ref
  (let [db   (om/tree->db Tree tree-data true)
        tree (om/db->tree (-> (om/get-query Tree) ffirst second)
               (:tree db) db)]
    (is (= tree (:tree tree-data)))))
