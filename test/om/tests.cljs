(ns om.tests
  (:require [cljs.test :refer-macros [is are deftest run-tests]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defprotocol IFoo
  (-foo [this]))

(defn derive* [cursor]
  (specify cursor
    om/ICursorDerive
    (-derive [this derived state path]
      (derive* (om/to-cursor derived state path)))
    IFoo
    (-foo [_] :foo)))

(deftest cursor-protocols 
  (is (om/cursor? (om/to-cursor [1 2 3])))
  (is (om/cursor? (om/to-cursor {:foo "bar"})))
  (are [x y] (= x y)
       (.-value (om/to-cursor [1 2 3])) [1 2 3]
       (.-value (om/to-cursor {:foo "bar"})) {:foo "bar"}
       (first (om/to-cursor [1 2 3])) 1
       (first (om/to-cursor {:foo "bar"})) [:foo "bar"]
       (:foo (first (om/to-cursor [{:foo "bar"}]))) "bar"
       (.-path (first (om/to-cursor [{:foo "bar"}]))) [0]
       (.-path (first (rest (om/to-cursor [{:foo "bar"} {:baz "woz"}])))) [1]
       (rest (rest (om/to-cursor [{:foo "bar"} {:baz "woz"}]))) ()
       (.-path (first (next (om/to-cursor [{:foo "bar"} {:baz "woz"}])))) [1]
       (.-path (get-in (om/to-cursor {:foo [{:id 1}]}) [:foo 0])) [:foo 0]
       (get-in (om/to-cursor {:foo [{:id 1}]}) [:foo 0 :id]) 1
       (assoc (om/to-cursor {:foo 1}) :bar 2) {:foo 1 :bar 2}
       {:foo 1 :bar 2} (assoc (om/to-cursor {:foo 1}) :bar 2)
       (dissoc (om/to-cursor {:foo 1}) :foo) {}
       {} (dissoc (om/to-cursor {:foo 1}) :foo)
       (map identity (om/to-cursor [{:id 1} {:id 2} {:id 3}]))
       [{:id 1} {:id 2} {:id 3}]
       [{:id 1} {:id 2} {:id 3}]
       (map identity (om/to-cursor [{:id 1} {:id 2} {:id 3}])))
  (let [c (derive* (om/to-cursor {:foo {:bar {:baz 'woz}}} [] nil))]
    (is (= (-foo (get-in c [:foo :bar])) :foo))))

(run-tests)
