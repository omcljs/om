(ns om.tests
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println "Cursor tests")

(defprotocol IFoo
  (-foo [this]))

(defn derive* [cursor]
  (specify cursor
    om/ICursorDerive
    (-derive [this derived state path]
      (derive* (om/to-cursor derived state path)))
    IFoo
    (-foo [_] :foo)))

(defn run-tests []
  (assert (om/cursor? (om/to-cursor [1 2 3])))
  (assert (= (.-value (om/to-cursor [1 2 3])) [1 2 3]))
  (assert (om/cursor? (om/to-cursor {:foo "bar"})))
  (assert (= (.-value (om/to-cursor {:foo "bar"})) {:foo "bar"}))
  (assert (= (first (om/to-cursor [1 2 3])) 1))
  (assert (= (first (om/to-cursor {:foo "bar"})) [:foo "bar"]))
  (assert (= (:foo (first (om/to-cursor [{:foo "bar"}]))) "bar"))
  (assert (= (.-path (first (om/to-cursor [{:foo "bar"}]))) [0]))
  (assert (= (.-path (first (rest (om/to-cursor [{:foo "bar"} {:baz "woz"}])))) [1]))
  (assert (= (rest (rest (om/to-cursor [{:foo "bar"} {:baz "woz"}]))) ()))
  (assert (= (.-path (first (next (om/to-cursor [{:foo "bar"} {:baz "woz"}])))) [1]))
  (assert (= (.-path (get-in (om/to-cursor {:foo [{:id 1}]}) [:foo 0])) [:foo 0]))
  (assert (= (get-in (om/to-cursor {:foo [{:id 1}]}) [:foo 0 :id]) 1))
  (assert (= (assoc (om/to-cursor {:foo 1}) :bar 2) {:foo 1 :bar 2}))
  (assert (= {:foo 1 :bar 2} (assoc (om/to-cursor {:foo 1}) :bar 2)))
  (assert (= (dissoc (om/to-cursor {:foo 1}) :foo) {}))
  (assert (= {} (dissoc (om/to-cursor {:foo 1}) :foo)))
  (assert (= (map identity (om/to-cursor [{:id 1} {:id 2} {:id 3}]))
            [{:id 1} {:id 2} {:id 3}]))
  (assert (= [{:id 1} {:id 2} {:id 3}]
            (map identity (om/to-cursor [{:id 1} {:id 2} {:id 3}]))))
  (let [c (derive* (om/to-cursor {:foo {:bar {:baz 'woz}}} [] nil))]
    (assert (= (-foo (get-in c [:foo :bar])) :foo))))

(run-tests)

(println "Tests completed without error")
