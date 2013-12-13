(ns om.core
  (:require React
            [om.vars :as vars]
            [om.dom :as dom :include-macros true]))

(def refresh-queued false)

(defn root [value f target]
  (let [state (if (instance? Atom value)
                value
                (atom value))
        rootf (fn []
                (set! refresh-queued false)
                (binding [vars/*state* state]
                  (dom/render
                    (dom/pure @state (f @state []))
                    target)))]
    (add-watch state ::root
      (fn [_ _ _ _]
        (when-not refresh-queued
          (set! refresh-queued true)
          (if (exists? js/requestAnimationFrame)
            (js/requestAnimationFrame rootf)
            (js/setTimeout rootf 16)))))
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
