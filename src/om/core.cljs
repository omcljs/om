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
                (dom/render
                  (dom/pure @state (f (with-meta @state {::path [] ::state state})))
                  target))]
    (add-watch state ::root
      (fn [_ _ _ _]
        (when-not refresh-queued
          (set! refresh-queued true)
          (if (exists? js/requestAnimationFrame)
            (js/requestAnimationFrame rootf)
            (js/setTimeout rootf 16)))))
    (rootf)))

(defn render
  ([f data] (render f data ::no-key))
  ([f data key]
    (if (keyword-identical? key ::no-key)
      (dom/pure data (f data path))
      (let [data (get data key)
            new-path (conj path key)]
        (dom/pure data
          (f (vary-meta data assoc ::path new-path)))))))

(defn bind [f data]
  (let [data (vary-meta data assoc ::owner vars/*owner*)]
    (fn [e] (f e data))))

(defn update! [data f k val]
  (let [m (meta data)]
    (swap! (::state meta) update-in (conj (-) k) f val)))
