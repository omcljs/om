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
                (dom/render
                  (dom/pure #js {:value @state}
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
  ([f data] (render f data nil))
  ([f data sorm]
    (cond
      (nil? sorm)
      (dom/pure #js {:value data} (f data))

      (sequential? sorm)
      (let [data'  (get-in data sorm)
            mdata' (with-meta data' (update-in (meta data) [::path] into sorm))]
        (dom/pure #js {:value data'} (f mdata')))

      :else
      (let [{:keys [path key opts]} sorm
            dataf  (get sorm :fn)
            data'  (get-in data path)
            data'  (if-not (nil? dataf)
                     (dataf data')
                     data')
            rkey   (get data' key)
            mdata' (with-meta data' (update-in (meta data) [::path] into path))]
        (dom/pure #js {:value data' :key rkey}
          (if (nil? opts)
            (f mdata')
            (f mdata' opts)))))))

(defn replace!
  ([data v]
    (let [m (meta data)]
      (swap! (::state m) assoc-in (::path m) v)))
  ([data ks v]
    (let [m (meta data)]
      (swap! (::state m) assoc-in (into (::path m) ks) v))))

(defn update!
  ([data ks f]
    (let [m (meta data)]
      (swap! (::state m) update-in (into (::path m) ks) f)))
  ([data ks f a]
    (let [m (meta data)]
      (swap! (::state m) update-in (into (::path m) ks) f a)))
  ([data ks f a b]
    (let [m (meta data)]
      (swap! (::state m) update-in (into (::path m) ks) f a b)))
  ([data ks f a b c]
    (let [m (meta data)]
      (swap! (::state m) update-in (into (::path m) ks) f a b c)))
  ([data ks f a b c d]
    (let [m (meta data)]
      (swap! (::state m) update-in (into (::path m) ks) f a b c d)))
  ([data ks f a b c d & args]
    (let [m (meta data)]
      (apply swap! (::state m) update-in (into (::path m) ks) f a b c d args))))
