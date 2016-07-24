(ns om.devcards.tutorials
  (:require [cljs.test :refer-macros [is async]]
            [devcards.core :refer-macros [defcard deftest dom-node]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

;; =============================================================================
;; Quick Start

(def animals-app-state
  (atom
    {:app/title "Animals"
     :animals/list
     [[1 "Ant"] [2 "Antelope"] [3 "Bird"] [4 "Cat"] [5 "Dog"]
      [6 "Lion"] [7 "Mouse"] [8 "Monkey"] [9 "Snake"] [10 "Zebra"]]}))

(defmulti animals-read (fn [env key params] key))

(defmethod animals-read :default
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defmethod animals-read :animals/list
  [{:keys [state] :as env} key {:keys [start end]}]
  {:value (subvec (:animals/list @state) start end)})

(defui AnimalsList
  static om/IQueryParams
  (params [this]
    {:start 0 :end 10})
  static om/IQuery
  (query [this]
    '[:app/title (:animals/list {:start ?start :end ?end})])
  Object
  (render [this]
    (let [{:keys [app/title animals/list]} (om/props this)]
      (dom/div nil
        (dom/h2 nil title)
        (apply dom/ul nil
          (map
            (fn [[i name]]
              (dom/li nil (str i ". " name)))
            list))))))

(def animals-reconciler
  (om/reconciler
    {:state animals-app-state
     :parser (om/parser {:read animals-read})}))

(defcard animals
  (dom-node
    (fn [_ node]
      (om/add-root! animals-reconciler AnimalsList node))))

;; =============================================================================
;; Componentes, Identity & Normalization

(enable-console-print!)

(def cian-init-data
  {:list/one [{:name "John" :points 0}
              {:name "Mary" :points 0}
              {:name "Bob"  :points 0}]
   :list/two [{:name "Mary" :points 0 :age 27}
              {:name "Gwen" :points 0}
              {:name "Jeff" :points 0}]})

;; -----------------------------------------------------------------------------
;; Parsing

(defmulti cian-read om/dispatch)

(defn get-people [state key]
  (let [st @state]
    (into [] (map #(get-in st %)) (get st key))))

(defmethod cian-read :list/one
  [{:keys [state] :as env} key params]
  {:value (get-people state key)})

(defmethod cian-read :list/two
  [{:keys [state] :as env} key params]
  {:value (get-people state key)})

(defmulti cian-mutate om/dispatch)

(defmethod cian-mutate 'points/increment
  [{:keys [state]} _ {:keys [name]}]
  {:action
   (fn []
     (swap! state update-in
       [:person/by-name name :points]
       inc))})

(defmethod cian-mutate 'points/decrement
  [{:keys [state]} _ {:keys [name]}]
  {:action
   (fn []
     (swap! state update-in
       [:person/by-name name :points]
       #(let [n (dec %)] (if (neg? n) 0 n))))})

;; -----------------------------------------------------------------------------
;; Components

(defui Person
  static om/Ident
  (ident [this {:keys [name]}]
    [:person/by-name name])
  static om/IQuery
  (query [this]
    '[:name :points :age])
  Object
  (render [this]
    (println "Render Person" (-> this om/props :name))
    (let [{:keys [points name foo] :as props} (om/props this)]
      (dom/li nil
        (dom/label nil (str name ", points: " points))
        (dom/button
          #js {:onClick
               (fn [e]
                 (om/transact! this
                   `[(points/increment ~props)]))}
          "+")
        (dom/button
          #js {:onClick
               (fn [e]
                 (om/transact! this
                   `[(points/decrement ~props)]))}
          "-")))))

(def person (om/factory Person {:keyfn :name}))

(defui ListView
  Object
  (render [this]
    (println "Render ListView" (-> this om/path first))
    (let [list (om/props this)]
      (apply dom/ul nil
        (map person list)))))

(def list-view (om/factory ListView))

(defui RootView
  static om/IQuery
  (query [this]
    (let [subquery (om/get-query Person)]
      `[{:list/one ~subquery} {:list/two ~subquery}]))
  Object
  (render [this]
    (println "Render RootView")
    (let [{:keys [list/one list/two]} (om/props this)]
      (apply dom/div nil
        [(dom/h2 nil "List A")
         (list-view one)
         (dom/h2 nil "List B")
         (list-view two)]))))

(def cian-reconciler
  (om/reconciler
    {:state  cian-init-data
     :parser (om/parser {:read cian-read :mutate cian-mutate})}))

(defcard component-identity-and-normalization
  (dom-node
    (fn [_ node]
      (om/add-root! cian-reconciler RootView node))))

;; =============================================================================
;; Thinking With Links!

(def links-init-data
  {:current-user {:email "bob.smith@gmail.com"}
   :items [{:id 0 :title "Foo"}
           {:id 1 :title "Bar"}
           {:id 2 :title "Baz"}]})

(defmulti links-read om/dispatch)

(defmethod links-read :items
  [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defui LinksItem
  static om/Ident
  (ident [_ {:keys [id]}]
    [:item/by-id id])
  static om/IQuery
  (query [_]
    '[:id :title [:current-user _]])
  Object
  (render [this]
    (let [{:keys [title current-user]} (om/props this)]
      (dom/li nil
        (dom/div nil title)
        (dom/div nil (:email current-user))))))

(def links-item (om/factory LinksItem))

(defui LinksSomeList
  static om/IQuery
  (query [_]
    [{:items (om/get-query LinksItem)}])
  Object
  (render [this]
    (dom/div nil
      (dom/h2 nil "A List!")
      (dom/ul nil
        (map links-item (-> this om/props :items))))))

(def links-reconciler
  (om/reconciler
    {:state links-init-data
     :parser (om/parser {:read links-read})}))

(defcard links
  (dom-node
    (fn [_ node]
      (om/add-root! links-reconciler LinksSomeList node))))

;; =============================================================================
;; Queries with unions

(def union-init-data
  {:dashboard/items
   [{:id 0 :type :dashboard/post
     :author "Laura Smith"
     :title "A Post!"
     :content "Lorem ipsum dolor sit amet, quem atomorum te quo"
     :favorites 0}
    {:id 1 :type :dashboard/photo
     :title "A Photo!"
     :image "photo.jpg"
     :caption "Lorem ipsum"
     :favorites 0}
    {:id 2 :type :dashboard/post
     :author "Jim Jacobs"
     :title "Another Post!"
     :content "Lorem ipsum dolor sit amet, quem atomorum te quo"
     :favorites 0}
    {:id 3 :type :dashboard/graphic
     :title "Charts and Stufff!"
     :image "chart.jpg"
     :favorites 0}
    {:id 4 :type :dashboard/post
     :author "May Fields"
     :title "Yet Another Post!"
     :content "Lorem ipsum dolor sit amet, quem atomorum te quo"
     :favorites 0}]})

(defui Post
  static om/IQuery
  (query [this]
    [:id :type :title :author :content])
  Object
  (render [this]
    (let [{:keys [title author content] :as props} (om/props this)]
      (dom/div nil
        (dom/h3 nil title)
        (dom/h4 nil author)
        (dom/p nil content)))))

(def post (om/factory Post))

(defui Photo
  static om/IQuery
  (query [this]
    [:id :type :title :image :caption])
  Object
  (render [this]
    (let [{:keys [title image caption]} (om/props this)]
      (dom/div nil
        (dom/h3 nil (str "Photo: " title))
        (dom/div nil image)
        (dom/p nil "Caption: ")))))

(def photo (om/factory Photo))

(defui Graphic
  static om/IQuery
  (query [this]
    [:id :type :title :image])
  Object
  (render [this]
    (let [{:keys [title image]} (om/props this)]
      (dom/div nil
        (dom/h3 nil (str "Graphic: " title))
        (dom/div nil image)))))

(def graphic (om/factory Graphic))

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
         (om/get-query Graphic)])))
  Object
  (render [this]
    (let [{:keys [id type favorites] :as props} (om/props this)]
      (dom/li
        #js {:style #js {:padding 10 :borderBottom "1px solid black"}}
        (dom/div nil
          (({:dashboard/post    post
             :dashboard/photo   photo
             :dashboard/graphic graphic} type)
            (om/props this)))
        (dom/div nil
          (dom/p nil (str "Favorites: " favorites))
          (dom/button
            #js {:onClick
                 (fn [e]
                   (om/transact! this
                     `[(dashboard/favorite {:ref [~type ~id]})]))}
            "Favorite!"))))))

(def dashboard-item (om/factory DashboardItem))

(defui Dashboard
  static om/IQuery
  (query [this]
    [{:dashboard/items (om/get-query DashboardItem)}])
  Object
  (render [this]
    (let [{:keys [dashboard/items]} (om/props this)]
      (apply dom/ul
        #js {:style #js {:padding 0}}
        (map dashboard-item items)))))

(defmulti union-read om/dispatch)

(defmethod union-read :dashboard/items
  [{:keys [state]} k _]
  (let [st @state]
    {:value (into [] (map #(get-in st %)) (get st k))}))

(defmulti mutate om/dispatch)

(defmethod mutate 'dashboard/favorite
  [{:keys [state]} k {:keys [ref]}]
  {:action
   (fn []
     (swap! state update-in (conj ref :favorites) inc))})

(def union-reconciler
  (om/reconciler
    {:state  union-init-data
     :parser (om/parser {:read union-read :mutate mutate})}))

(defcard union
  (dom-node
    (fn [_ node]
      (om/add-root! union-reconciler Dashboard node))))
