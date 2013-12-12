(ns om.core
  (:require React [om.dom :as dom :include-macros true]))

(def ^:dynamic *state* nil)

(defn root [value f target]
  (let [state (if (instance? Atom value)
                value
                (atom value))
        rootf (fn []
                (dom/render
                  (dom/pure @state
                    (binding [*state* state] (f @state []))) target))]
    (add-watch state ::root (fn [_ _] (rootf)))
    (rootf)))

(defn render [f data ks]
  (let [data' (get-in data ks)
        state *state*]
    (dom/pure data' (binding [*state* state] (f data' ks)))))

(defn update! [path f]
  (let [state *state*]
    (fn [e] (swap! state update-in path f))))
