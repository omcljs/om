(ns examples.two-lists.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom
    {:list0 [:a :b :c]
     :list1 [:d :e :f]
     :items
     {:a {:text "cat"}
      :b {:text "dog"}
      :c {:text "bird"}
      :d {:text "lion"}
      :e {:text "antelope"}
      :f {:text "zebra"}}}))

(defn removable [item id order]
  (specify item
    Object
    (remove
      ([this x]
         (om/transact! order #(vec (remove #{id} %)))))))

(defn collection [order data]
  (specify order
    ISeqable
    (-seq [_]
      (map #(removable (get data %) % order) order))
    ILookup
    (-lookup
      ([this k] (-lookup this k nil))
      ([this k not-found]
         (-nth this k not-found)))
    IIndexed
    (-nth
      ([this k] (-nth this k nil))
      ([this k not-found]
         (let [id (nth order k)]
           (removable (get data id) id order))))))

(defn item-view [aitem owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil
        (dom/div nil (str (:text aitem)))
        (dom/button
          #js {:onClick #(.remove aitem)}
          "Remove!")))))

(defn list-view [alist owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/ul nil (om/build-all item-view alist)))))

(defn my-app [global owner]
  (reify
    om/IRender
    (render [_]
      (let [items (:items global)
            ]
        (dom/div nil
          (dom/h2 nil "Two Lists")
          (om/build list-view (collection (:list0 global) items))
          (om/build list-view (collection (:list1 global) items)))))))

(om/root my-app app-state
  {:target (.getElementById js/document "app")})
