(ns om.core
  (:require React [om.dom :as dom :include-macros true]))

(def ^:dynamic *state* nil)
(def ^:dynamic *focus* nil)
(def ^:dynamic *path* nil)

(defn root [value f target]
  (let [state (atom value)
        rootf (fn []
                (binding [*state* state
                          *focus* @state
                          *path* []]
                  (dom/render (dom/pure *focus* (f)) target)))]
    (add-watch state (fn [_ _] (rootf)))
    (rootf)))

(defn render [f & ks]
  (binding [*focus* (get-in *focus* ks)]
    (pure *focus* (f *focus*))))

(defn => [f]
  (let [state *state*
        path  *path*
        focus *focus*]
    (fn [e] (f e {:state state :path path :focus focus}))))

(defn update! [key fn]
  (let [state *state*
        path  *path*]
    (fn [e] (swap! state update-in (conj path key) fn))))
