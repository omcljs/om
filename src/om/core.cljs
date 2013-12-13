(ns om.core
  (:require React
            [om.vars :as vars]
            [om.dom :as dom :include-macros true]))

(defn root [value f target]
  (let [state (if (instance? Atom value)
                value
                (atom value))
        rootf (fn []
                (bindings [vars/*state state]
                  (dom/render
                    (dom/pure @state (f @state []))
                    target)))]
    (add-watch state ::root (fn [_ _ _ _] (rootf)))
    (rootf)))

(defn render
  ([f data path] (render f data path ::no-key))
  ([f data path key]
    (if (keyword-identical? key ::no-key)
      (dom/pure data (f data path))
      (let [data (get data key)
            path (conj path key)]
        (dom/pure data (f data path))))))

(defn bind
  ([f] (bind f nil))
  ([f path]
    (let [state vars/*state*
          owner vars/*owner*
          m (if-not (nil? path)
              {:state state :owner owner :path path}
              {:state state :owner owner})]
      (fn [e] (f e m)))))

(defn update! [path f]
  (let [state vars/*state*]
    (fn [e] (swap! state update-in path f))))
