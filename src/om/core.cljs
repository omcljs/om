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
                  (dom/pure #js {:value @state :opts null}
                    (f (with-meta @state {::path [] ::state state})))
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
  ([f data key] (render data nil key))
  ([f data opts key]
    (if (keyword-identical? key ::no-key)
      (dom/pure #js {:value data :opts opts} (f data))
      (let [data (get data key)
            new-path (conj path key)]
        (dom/pure #js {:value data :opts opts}
          (f (vary-meta data assoc ::path new-path)))))))

(defn bind [f data]
  (let [data (vary-meta data assoc ::owner vars/*owner*)]
    (fn [e] (f e data))))

(defn update!
  ([data k f]
    (let [m (meta data)]
      (swap! (::state meta) update-in (conj (::path meta) k) f)))
  ([data k f a]
    (let [m (meta data)]
      (swap! (::state meta) update-in (conj (::path meta) k) f a)))
  ([data k f a b]
    (let [m (meta data)]
      (swap! (::state meta) update-in (conj (::path meta) k) f a b)))
  ([data k f a b c]
    (let [m (meta data)]
      (swap! (::state meta) update-in (conj (::path meta) k) f a b c)))
  ([data k f a b c d]
    (let [m (meta data)]
      (swap! (::state meta) update-in (conj (::path meta) k) f a b c d)))
  ([data k f a b c d & args]
    (let [m (meta data)]
      (apply swap! (::state meta) update-in (conj (::path meta) k) f a b c d args))))
