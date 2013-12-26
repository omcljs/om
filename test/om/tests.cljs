(ns om.tests
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println "Starting tests")

(assert (= (.-value (om/to-cursor [1 2 3])) [1 2 3]))
(assert (= (.-value (om/to-cursor {:foo "bar"})) {:foo "bar"}))
(assert (= (first (om/to-cursor [1 2 3])) 1))
(assert (= (first (om/to-cursor {:foo "bar"})) [:foo "bar"]))
(assert (= (:foo (first (om/to-cursor [{:foo "bar"}]))) "bar"))
(assert (= (.-path (first (om/to-cursor [{:foo "bar"}]))) [0]))

(println "Tests completed without error")
