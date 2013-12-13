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
                  (dom/pure #js {:value @state :opts nil}
                    (f (with-meta @state {::state state ::path []})))
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
  ([f data] (render f data nil ::no-key))
  ([f data key] (render f data nil key))
  ([f data opts key]
    (if (keyword-identical? key ::no-key)
      (dom/pure #js {:value data :opts opts} (f data))
      (let [data' (get data key)]
        (dom/pure #js {:value data' :opts opts}
          (f (with-meta data' (update-in (meta data) [::path] conj key))))))))

(defn bind [f data]
  (let [data (vary-meta data assoc ::owner vars/*owner*)]
    (fn [e] (f e data))))

(defn set! [data k v]
  (let [m (meta data)]
    (swap! (::state m) assoc-in (conj (::path m) k) v)))

(defn update!
  ([data k f]
    (let [m (meta data)]
      (swap! (::state m) update-in (conj (::path m) k) f)))
  ([data k f a]
    (let [m (meta data)]
      (swap! (::state m) update-in (conj (::path m) k) f a)))
  ([data k f a b]
    (let [m (meta data)]
      (swap! (::state m) update-in (conj (::path m) k) f a b)))
  ([data k f a b c]
    (let [m (meta data)]
      (swap! (::state m) update-in (conj (::path m) k) f a b c)))
  ([data k f a b c d]
    (let [m (meta data)]
      (swap! (::state m) update-in (conj (::path m) k) f a b c d)))
  ([data k f a b c d & args]
    (let [m (meta data)]
      (apply swap! (::state m) update-in (conj (::path m) k) f a b c d args))))
