(ns om.tests
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println "Starting tests")

(assert (om/cursor? (om/to-cursor [1 2 3])))
(assert (= (.-value (om/to-cursor [1 2 3])) [1 2 3]))
(assert (om/cursor? (om/to-cursor {:foo "bar"})))
(assert (= (.-value (om/to-cursor {:foo "bar"})) {:foo "bar"}))
(assert (= (first (om/to-cursor [1 2 3])) 1))
(assert (= (first (om/to-cursor {:foo "bar"})) [:foo "bar"]))
(assert (= (:foo (first (om/to-cursor [{:foo "bar"}]))) "bar"))
(assert (= (.-path (first (om/to-cursor [{:foo "bar"}]))) [0]))
(assert (= (.-path (get-in (om/to-cursor {:foo [{:id 1}]}) [:foo 0])) [:foo 0]))
(assert (= (get-in (om/to-cursor {:foo [{:id 1}]}) [:foo 0 :id]) 1))
(assert (= (assoc (om/to-cursor {:foo 1}) :bar 2) {:foo 1 :bar 2}))
(assert (= {:foo 1 :bar 2} (assoc (om/to-cursor {:foo 1}) :bar 2)))
(assert (= (map identity (om/to-cursor [{:id 1} {:id 2} {:id 3}]))
           [{:id 1} {:id 2} {:id 3}]))
(assert (= [{:id 1} {:id 2} {:id 3}]
           (map identity (om/to-cursor [{:id 1} {:id 2} {:id 3}]))))

(println "Tests completed without error")
