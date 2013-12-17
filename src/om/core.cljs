(ns om.core
  (:require React
            [om.dom :as dom :include-macros true]))

(def refresh-queued false)

(defn root [value f target]
  (let [state (if (instance? Atom value)
                value
                (atom value))
        rootf (fn []
                (set! refresh-queued false)
                (let [path []]
                  (dom/render
                    (dom/pure #js {:value @state}
                      (f (with-meta @state {::state state ::path path})))
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
  ([f cursor] (render f cursor nil))
  ([f cursor sorm]
    (cond
      (nil? sorm)
      (dom/pure #js {:value cursor} (f cursor))

      (sequential? sorm)
      (let [data    (get-in cursor sorm)
            cursor' (with-meta data (update-in (meta cursor) [::path] into sorm))]
        (dom/pure #js {:value data} (f cursor)))

      :else
      (let [{:keys [path key opts]} sorm
            dataf   (get sorm :fn)
            data    (get-in cursor path)
            data    (if-not (nil? dataf) (dataf data) data)
            rkey    (when-not (nil? key) (get data key))
            cursor' (with-meta data (update-in (meta cursor) [::path] into path))]
        (dom/pure #js {:value data :key rkey}
          (if (nil? opts)
            (f cursor')
            (f cursor' opts)))))))

(defn update!
  ([cursor f]
    (let [m (meta cursor)
          path (::path m)]
      (if (empty? path)
        (swap! (::state m) f)
        (swap! (::state m) update-in path f))))
  ([cursor ks f]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f)))
  ([cursor ks f a]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a)))
  ([cursor ks f a b]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b)))
  ([cursor ks f a b c]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b c)))
  ([cursor ks f a b c d]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b c d)))
  ([cursor ks f a b c d & args]
    (let [m (meta cursor)]
      (apply swap! (::state m) update-in (into (::path m) ks) f a b c d args))))
