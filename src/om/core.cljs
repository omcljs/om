(ns om.core
  (:require-macros [om.core :refer [pure component]])
  (:require [om.dom :as dom :include-macros true]))

;; =============================================================================
;; React Life Cycle Protocols
;;
;; http://facebook.github.io/react/docs/component-specs.html

(defprotocol IInitState
  (init-state [this owner]))

(defprotocol IShouldUpdate
  (should-update [this owner next-props next-state]))

(defprotocol IWillMount
  (will-mount [this owner]))

(defprotocol IDidMount
  (did-mount [this owner node]))

(defprotocol IWillUnmount
  (will-unmount [this owner]))

(defprotocol IWillUpdate
  (will-update [this owner next-props next-state]))

(defprotocol IDidUpdate
  (did-update [this owner prev-props prev-state root-node]))

(defprotocol IRender
  (render [this owner]))

;; =============================================================================
;; A Truly Pure Component
;; 
;; This React class takes an immutable value as its props and an instance that
;; must at a minimum implement IRender as its children.
;;
;; (Pure. {:foo "bar"} irender-instance)

(def ^:private Pure
  (js/React.createClass
    #js {:getInitialState
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               #js {:__om_state
                    (merge {}
                      (when (satisfies? IInitState c)
                        (init-state c this)))})))
         :shouldComponentUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (if (satisfies? IShouldUpdate c)
                 (should-update c this next-props next-state)
                 (or (not (identical? (aget (.-props this) "__om_value")
                                      (aget next-props "__om_value")))
                     ;; since we don't use setState, next-state not useful
                     (not (identical? (aget (.-state this) "__om_state")
                                      (aget (.-state this) "__om_pending_state"))))))))
         :componentWillMount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillMount c)
                 (will-mount c this)))))
         :componentDidMount
         (fn [node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidMount c)
                 (did-mount c this node)))))
         :componentWillUnmount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUnmount c)
                 (will-unmount c this)))))
         :componentWillUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUpdate c)
                 (will-update c this next-props next-state)))
             ;; merge any pending state
             (let [state (.-state this)]
               (when-let [pending-state (aget state "__om_pending_state")]
                 (doto state
                   (aset "__om_state" pending-state)
                   (aset "__om_pending_state" nil))))))
         :componentDidUpdate
         (fn [prev-props prev-state root-node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidUpdate c)
                 (did-update c this prev-props prev-state root-node)))))
         :render
         (fn []
           (this-as this
             (render (.. this -props -children) this)))}))

(def ^:private refresh-queued false)

(defn root
  "Takes an immutable value or value wrapped in an atom, an initial
   function f, and a DOM target. Installs an Om/React render loop. f
   must return an instance that at a minimum implements IRender (it
   may implement other React life cycle protocols). f must take a
   single argument which will be the root cursor. A cursor is simply
   data that has been annotated via metadata with state and path
   information - :om.core/state and :om.core/path respectively.

   Example:

   (root {:message :hello}
     (fn [data]
       ...)
     js/document.body)"
  [value f target]
  (let [state (if (instance? Atom value)
                value
                (atom value))
        rootf (fn []
                (set! refresh-queued false)
                (let [path []
                      state-value @state]
                  (dom/render
                    (pure #js {:__om_tvalue state-value
                               :__om_value state-value
                               :__om_app_state state
                               :__om_path path}
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

(defn build
  "Builds a Om component. Takes an IRender instance returning function
   f, a cursor, and an optional third argument which may be a vector
   representing the path or a map of build specifications. 

   f - is a function of two arguments the first argument will be the
   cursor and the second argument will be a map of optional values
   passed to build. f must return at a minimum an IRender instance,
   this instance may implement other React life cycle protocols.

   cursor - an immutable value annotated with :om.core/state and
   :om.core/path

   sorm - If a vector, specifies the relative path into the tree that
   the component will be built from. If a map the following
   keys are allowed:

     :path      - the relative path in the tree to build the component with
     :abs-path  - an absolute path from the root
     :key       - a keyword that should be used to look up the key used by
                  React itself when rendering sequential things.
     :react-key - an explicit react key
     :fn        - a function to apply to the data at the relative path before
                  invoking f.
     :opts      - a map of options to pass to the component.

   Example:

     (build list-of-gadgets cursor
        {:path [:gadgets]
         :opts {:event-chan ...
                :narble ...}})
  "
  ([f cursor] (build f cursor nil))
  ([f cursor sorm]
    (cond
      (nil? sorm)
      (let [m (meta cursor)]
        (pure #js {:__om_tvalue cursor
                   :__om_value cursor
                   :__om_app_state (::state m)
                   :__om_path (::path m)}
          (f cursor)))

      (sequential? sorm)
      (let [data    (get-in cursor sorm)
            m       (meta cursor)
            path    (into (::path m) sorm)
            cursor' (with-meta data (assoc m ::path path))]
        (pure #js {:__om_tvalue data
                   :__om_value data
                   :__om_app_state (::state m)
                   :__om_path path}
          (f cursor')))

      :else
      (let [{:keys [path key react-key opts]} sorm
            dataf   (get sorm :fn)
            path    (if (nil? path) (:abs-path sorm) path)
            data    (get-in cursor path)
            tdata   data
            data    (if-not (nil? dataf) (dataf data) data)
            rkey    (if-not (nil? key)
                      (get data key)
                      (if-not (nil? react-key)
                        react-key))
            m       (meta cursor)
            path    (if (contains? sorm :abs-path)
                      path
                      (into (::path m) path))
            cursor' (with-meta data (assoc m ::path path))]
        (pure #js {:__om_tvalue tdata
                   :__om_value data
                   :__om_app_state (::state m)
                   :__om_path path
                   :key rkey}
          (if (nil? opts)
            (f cursor')
            (f cursor' opts)))))))

(defn update!
  "Given a cursor, a list of keys ks, mutate the tree at the path
   specified by the cursor + the keys by applying f to the specified
   value in the tree. If only given two arguments, assumed no list
   of keys was specified. An Om re-render will be triggered."
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

(defn get-node
  "A helper function to get at React refs. Given a owning pure node
  extract the ref specified by name. Note the life cycle protocol methods
  all pass the owner as an argument, ie. IRender."
  [owner name]
  (when-let [refs (.-refs owner)]
    (.getDOMNode (aget refs name))))

(defn set-state!
  "Takes a pure owning component, a sequential list of keys and value and
   sets the state of the component. Conceptually analagous to React
   setState."
  [owner ks v]
  (let [props     (.-props owner)
        state     (.-state owner)
        app-state (aget props "__om_app_state")
        path      (aget props "__om_path")
        value     (aget props "__om_tvalue")
        pstate    (or (aget state "__om_pending_state")
                      (aget state "__om_state"))]
    (aset state "__om_pending_state" (assoc-in pstate ks v))
    ;; invalidate path to component
    (if-not (empty? path)
      (swap! app-state assoc-in path value)
      (reset! app-state value))))

(defn get-state
  "Takes a pure owning component and sequential list of keys and returns
   a property if it exists. Will never return pending state values."
  ([owner] (aget (.-state owner) "__om_state"))
  ([owner ks]
    (if-not (empty? ks)
      (get-in (aget (.-state owner) "__om_state") ks)
      (get-state owner))))
