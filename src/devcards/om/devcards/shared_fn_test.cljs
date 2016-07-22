(ns om.devcards.shared-fn-test
  (:require [devcards.core :refer-macros [defcard deftest dom-node]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui Home
  static om/IQuery
  (query [this] [:counter])

  Object
  (render [this]
    (let [shared (om/shared this)
          props  (om/props this)]
      (println "Render Root!")
      (dom/div nil
        (dom/h3 nil (str "Props: " props))
        (dom/h3 nil (str "Shared: " shared))
        (dom/button
          #js {:onClick #(om/transact! this '[(my/test) :counter])}
          "Increment!")))))

(def app-state (atom {:counter 0}))

(defn read
  [env key params]
  (let [{:keys [state]} env]
    {:value (get @state key)}))

(defn mutate
  [env key params]
  (let [{:keys [state]} env]
    {:value  {:keys [:counter]}
     :action #(swap! state update-in [:counter] inc)}))

(def reconciler
  (om/reconciler
    {:state     app-state
     :parser    (om/parser {:read read :mutate mutate})
     :shared    {}
     :shared-fn (fn [root-props]
                  root-props)}))

(defcard test-om-478
  "Test that re-running shared-fn works"
  (dom-node
    (fn [_ node]
      (om/add-root! reconciler Home node))))
