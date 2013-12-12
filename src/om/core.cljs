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
  (let [state *state*]
    (dom/pure data (binding [*state* state] (f data ks)))))

(defn bind
  ([f] (bind f nil))
  ([f path]
    (let [state *state*
          owner dom/*owner*
          m (if path
              {:state state :owner owner :path path}
              {:state state :owner owner})]
      (fn [e] (f e m)))))

(defn update! [path f]
  (let [state *state*]
    (fn [e] (swap! state update-in path f))))
