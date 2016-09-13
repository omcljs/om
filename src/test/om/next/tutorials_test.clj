(ns om.next.tutorials-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [deftest testing is are]]
            [om.test-utils :refer [remove-whitespace]]
            [om.next :as om :refer [defui]]
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

(deftest test-render-animals-tutorial
  (let [result-markup (remove-whitespace
                        "<div data-reactroot=\"\" data-reactid=\"1\">
                           <h2 data-reactid=\"2\">Animals</h2>
                           <ul data-reactid=\"3\">
                             <li data-reactid=\"4\">1. Ant</li>
                             <li data-reactid=\"5\">2. Antelope</li>
                             <li data-reactid=\"6\">3. Bird</li>
                             <li data-reactid=\"7\">4. Cat</li>
                             <li data-reactid=\"8\">5. Dog</li>
                             <li data-reactid=\"9\">6. Lion</li>
                             <li data-reactid=\"10\">7. Mouse</li>
                             <li data-reactid=\"11\">8. Monkey</li>
                             <li data-reactid=\"12\">9. Snake</li>
                             <li data-reactid=\"13\">10. Zebra</li>
                           </ul>
                         </div>")]
    (testing "render with factory"
      (let [ctor (om/factory AnimalsList)]
        (is (= (str (#'dom/render-to-str* (ctor @animals-app-state))) result-markup))))
    (testing "render with reconciler & add-root!"
      (let [c (om/add-root! animals-reconciler AnimalsList nil)
            markup-str (str (#'dom/render-to-str* c))]
        (is (= (om/react-type (om/app-root animals-reconciler)) AnimalsList))
        (is (= markup-str result-markup))))))

;; =============================================================================
;; Om Links tutorial

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

(deftest test-render-links-tutorial
  (let [c (om/add-root! links-reconciler LinksSomeList nil)]
    (is (= (str (#'dom/render-to-str* c))
           (remove-whitespace
             "<div data-reactroot=\"\" data-reactid=\"1\">
                <h2 data-reactid=\"2\">A List!</h2>
                <ul data-reactid=\"3\">
                  <li data-reactid=\"4\">
                    <div data-reactid=\"5\">Foo</div>
                    <div data-reactid=\"6\">bob.smith@gmail.com</div>
                  </li>
                  <li data-reactid=\"7\">
                    <div data-reactid=\"8\">Bar</div>
                    <div data-reactid=\"9\">bob.smith@gmail.com</div>
                  </li>
                  <li data-reactid=\"10\">
                    <div data-reactid=\"11\">Baz</div>
                    <div data-reactid=\"12\">bob.smith@gmail.com</div>
                  </li>
                </ul>
              </div>")))))

;; =============================================================================
;; Componentes, Identity & Normalization

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
    ;(println "Render ListView" (-> this om/path first))
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

(deftest test-cian-tutorial
  (let [c (om/add-root! cian-reconciler RootView nil)]
    (is (= (str (#'dom/render-to-str* c))
           (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                 <h2 data-reactid=\"2\">List A</h2>
                                 <ul data-reactid=\"3\">
                                   <li data-reactid=\"4\">
                                     <label data-reactid=\"5\">John, points: 0</label>
                                     <button data-reactid=\"6\">+</button>
                                     <button data-reactid=\"7\">-</button>
                                   </li>
                                   <li data-reactid=\"8\">
                                     <label data-reactid=\"9\">Mary, points: 0</label>
                                     <button data-reactid=\"10\">+</button>
                                     <button data-reactid=\"11\">-</button>
                                   </li>
                                   <li data-reactid=\"12\">
                                     <label data-reactid=\"13\">Bob, points: 0</label>
                                     <button data-reactid=\"14\">+</button>
                                     <button data-reactid=\"15\">-</button>
                                   </li>
                                 </ul>
                                 <h2 data-reactid=\"16\">List B</h2>
                                 <ul data-reactid=\"17\">
                                   <li data-reactid=\"18\">
                                     <label data-reactid=\"19\">Mary, points: 0</label>
                                     <button data-reactid=\"20\">+</button>
                                     <button data-reactid=\"21\">-</button>
                                   </li>
                                   <li data-reactid=\"22\">
                                     <label data-reactid=\"23\">Gwen, points: 0</label>
                                     <button data-reactid=\"24\">+</button>
                                     <button data-reactid=\"25\">-</button>
                                   </li>
                                   <li data-reactid=\"26\">
                                     <label data-reactid=\"27\">Jeff, points: 0</label>
                                     <button data-reactid=\"28\">+</button>
                                     <button data-reactid=\"29\">-</button>
                                   </li>
                                 </ul>
                               </div>")))))

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


(deftest test-unions-tutorial
    (let [c (om/add-root! union-reconciler Dashboard nil)]
      (is (= (str (#'dom/render-to-str* c))
             (remove-whitespace
               "<ul style=\"padding:0;\" data-reactroot=\"\" data-reactid=\"1\">
                 <li style=\"padding:10px;border-bottom:1px solid black;\" data-reactid=\"2\">
                   <div data-reactid=\"3\">
                     <div data-reactid=\"4\">
                       <h3 data-reactid=\"5\">A Post!</h3>
                       <h4 data-reactid=\"6\">Laura Smith</h4>
                       <p data-reactid=\"7\">Lorem ipsum dolor sit amet, quem atomorum te quo</p>
                     </div>
                   </div>
                   <div data-reactid=\"8\">
                     <p data-reactid=\"9\">Favorites: 0</p>
                     <button data-reactid=\"10\">Favorite!</button>
                   </div>
                 </li>
                 <li style=\"padding:10px;border-bottom:1px solid black;\" data-reactid=\"11\">
                   <div data-reactid=\"12\">
                     <div data-reactid=\"13\">
                       <h3 data-reactid=\"14\">Photo: A Photo!</h3>
                       <div data-reactid=\"15\">photo.jpg</div>
                       <p data-reactid=\"16\">Caption: </p>
                     </div>
                   </div>
                   <div data-reactid=\"17\">
                     <p data-reactid=\"18\">Favorites: 0</p>
                     <button data-reactid=\"19\">Favorite!</button>
                   </div>
                 </li>
                 <li style=\"padding:10px;border-bottom:1px solid black;\" data-reactid=\"20\">
                   <div data-reactid=\"21\">
                     <div data-reactid=\"22\">
                       <h3 data-reactid=\"23\">Another Post!</h3>
                       <h4 data-reactid=\"24\">Jim Jacobs</h4>
                       <p data-reactid=\"25\">Lorem ipsum dolor sit amet, quem atomorum te quo</p>
                     </div>
                   </div>
                   <div data-reactid=\"26\">
                     <p data-reactid=\"27\">Favorites: 0</p>
                     <button data-reactid=\"28\">Favorite!</button>
                   </div>
                 </li>
                 <li style=\"padding:10px;border-bottom:1px solid black;\" data-reactid=\"29\">
                   <div data-reactid=\"30\">
                     <div data-reactid=\"31\">
                       <h3 data-reactid=\"32\">Graphic: Charts and Stufff!</h3>
                       <div data-reactid=\"33\">chart.jpg</div>
                     </div>
                   </div>
                   <div data-reactid=\"34\">
                     <p data-reactid=\"35\">Favorites: 0</p>
                     <button data-reactid=\"36\">Favorite!</button>
                   </div>
                 </li>
                 <li style=\"padding:10px;border-bottom:1px solid black;\" data-reactid=\"37\">
                   <div data-reactid=\"38\">
                     <div data-reactid=\"39\">
                       <h3 data-reactid=\"40\">Yet Another Post!</h3>
                       <h4 data-reactid=\"41\">May Fields</h4>
                       <p data-reactid=\"42\">Lorem ipsum dolor sit amet, quem atomorum te quo</p>
                     </div>
                   </div>
                   <div data-reactid=\"43\">
                     <p data-reactid=\"44\">Favorites: 0</p>
                     <button data-reactid=\"45\">Favorite!</button>
                   </div>
                 </li>
               </ul>")))))
