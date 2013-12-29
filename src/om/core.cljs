(ns om.core
  (:require-macros [om.core :refer [pure component check allow-reads]])
  (:require [om.dom :as dom :include-macros true]))

(def ^{:tag boolean :dynamic true} *read-enabled* false)

;; =============================================================================
;; React Life Cycle Protocols
;;
;; http://facebook.github.io/react/docs/component-specs.html

(defprotocol IInitState
  (init-state [this]))

(defprotocol IShouldUpdate
  (should-update [this next-props next-state]))

(defprotocol IWillMount
  (will-mount [this]))

(defprotocol IDidMount
  (did-mount [this node]))

(defprotocol IWillUnmount
  (will-unmount [this]))

(defprotocol IWillUpdate
  (will-update [this next-props next-state]))

(defprotocol IDidUpdate
  (did-update [this prev-props prev-state root-node]))

(defprotocol IRender
  (render [this]))

;; =============================================================================
;; A Truly Pure Component
;; 
;; This React class takes an immutable value as its props and an instance that
;; must at a minimum implement IRender as its children.
;;
;; (Pure. {:foo "bar"} irender-instance)

(defn children [node]
  (let [c (.. node -props -children)]
    (if (fn? c)
      (set! (.. node -props -children) (allow-reads (c node)))
      c)))

(defn get-props
  "Given an owning Pure node return the Om props. Analogous to React 
   component props."
  [x]
  (let [ret {:value (aget (.-props x) "__om_cursor" "value")}]
    (if-let [idx (aget (.-props x) "__om_index")]
      (assoc ret :index idx)
      ret)))

(def ^:private Pure
  (js/React.createClass
    #js {:getInitialState
         (fn []
           (this-as this
             (let [c (children this)]
               #js {:__om_state
                    (merge {}
                      (when (satisfies? IInitState c)
                        (allow-reads (init-state c))))})))
         :shouldComponentUpdate
         (fn [next-props next-state]
           (this-as this
             (let [props (.-props this)
                   c     (children this)]
               (if (satisfies? IShouldUpdate c)
                 (allow-reads
                   (should-update c
                     (get-props #js {:props next-props})
                     (aget (.-state this) "__om_pending_state")))
                 (cond
                   (not (identical? (.-value (aget props "__om_cursor"))
                                    (.-value (aget next-props "__om_cursor"))))
                   true
                   (not (identical? (aget (.-state this) "__om_state")
                                    (aget (.-state this) "__om_pending_state")))
                   true

                   (not (== (aget props "__om_index") (aget next-props "__om_index")))
                   true

                   :else false)))))
         :componentWillMount
         (fn []
           (this-as this
             (let [c (children this)]
               (when (satisfies? IWillMount c)
                 (allow-reads (will-mount c))))))
         :componentDidMount
         (fn [node]
           (this-as this
             (let [c (children this)]
               (when (satisfies? IDidMount c)
                 (allow-reads (did-mount c node))))))
         :componentWillUnmount
         (fn []
           (this-as this
             (let [c (children this)]
               (when (satisfies? IWillUnmount c)
                 (allow-reads (will-unmount c))))))
         :componentWillUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (children this)]
               (when (satisfies? IWillUpdate c)
                 (allow-reads
                   (will-update c
                     (get-props #js {:props next-props})
                     (aget (.-state this) "__om_pending_state")))))
             ;; merge any pending state
             (let [state (.-state this)]
               (when-let [pending-state (aget state "__om_pending_state")]
                 (doto state
                   (aset "__om_prev_state" (aget state "__om_state"))
                   (aset "__om_state" pending-state)
                   (aset "__om_pending_state" nil))))))
         :componentDidUpdate
         (fn [prev-props prev-state root-node]
           (this-as this
             (let [c (children this)]
               (when (satisfies? IDidUpdate c)
                 (allow-reads
                   (did-update c
                     (get-props #js {:props prev-props})
                     (aget (.-state this) "__om_prev_state")
                     root-node)))
               (aset (.-state this) "__om_prev_state" nil))))
         :render
         (fn []
           (this-as this
             (allow-reads
               (render (children this)))))}))

;; =============================================================================
;; Cursors

(declare to-cursor)

(defprotocol ICursor)

(defn cursor? [x]
  (satisfies? ICursor x))

(deftype MapCursor [value state path]
  ICursor
  ICounted
  (-count [_]
    (check (-count value)))
  ICollection
  (-conj [_ o]
    (check (MapCursor. (-conj value o) state path)))
  ILookup
  (-lookup [this k]
    (-lookup this k nil))
  (-lookup [_ k not-found]
    (check
      (let [v (-lookup value k not-found)]
        (if-not (= v not-found)
          (to-cursor v state (conj path k))
          not-found))))
  ISeqable
  (-seq [this]
    (check (map (fn [[k v]] [k (to-cursor v state (conj path k))]) value)))
  IAssociative
  (-contains-key? [_ k]
    (check (-contains-key? value k)))
  (-assoc [_ k v]
    (check (MapCursor. (-assoc value k v) state path)))
  IMap
  (-dissoc [_ k]
    (check (MapCursor. (-dissoc value k) state path)))
  IEquiv
  (-equiv [_ other]
    (check
      (if (cursor? other)
        (and (= value (.-value other))
             (= state (.-state other))
             (= path (.-path other)))
        (= value other))))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (check (-pr-writer value writer opts))))

(deftype VectorCursor [value state path]
  ICursor
  ISequential
  ICounted
  (-count [_]
    (check (-count value)))
  ICollection
  (-conj [_ o]
    (check (VectorCursor. (-conj value o) state path)))
  ILookup
  (-lookup [this n]
    (check (-nth this n nil)))
  (-lookup [this n not-found]
    (check (-nth this n not-found)))
  IIndexed
  (-nth [_ n]
    (check (to-cursor (-nth value n) state (conj path n))))
  (-nth [_ n not-found]
    (check
      (if (< n (-count value))
        (to-cursor (-nth value n) state (conj path n))
        not-found)))
  ISeqable
  (-seq [this]
    (check
      (when (pos? (count value))
        (map (fn [v i] (to-cursor v state (conj path i))) value (range)))))
  IAssociative
  (-contains-key? [_ k]
    (check (-contains-key? value k)))
  (-assoc [_ n v]
    (check (to-cursor (-assoc-n value n v) state path)))
  IStack
  (-peek [_]
    (check (to-cursor (-peek value) state path)))
  (-pop [_]
    (check (to-cursor (-pop value) state path)))
  IEquiv
  (-equiv [_ other]
    (check
      (if (cursor? other)
        (and (= value (.-value other))
             (= state (.-state other))
             (= path (.-path other)))
        (= value other))))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (check (-pr-writer value writer opts))))

(defn to-cursor
  ([val] (to-cursor val nil []))
  ([val state] (to-cursor val state []))
  ([val state path]
    (cond
      (cursor? val) val
      (map? val) (MapCursor. val state path)
      (vector? val) (VectorCursor. val state path)
      :else val)))

;; =============================================================================
;; API

(def ^:private refresh-queued false)

(defn root
  "Takes an immutable value or value wrapped in an atom, an initial
   function f, and a DOM target. Installs an Om/React render loop. f
   must return an instance that at a minimum implements IRender (it
   may implement other React life cycle protocols). f must take a
   single argument which will be the root cursor. A cursor is just
   the original data wrapped in an ICursor instance which maintains
   path information.

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
                (let [path   []
                      value  @state
                      cursor (to-cursor value state)]
                  (dom/render
                    (pure #js {:__om_cursor cursor}
                      (fn [this] (allow-reads (f cursor this))))
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
   f, a cursor, and an optional third argument which may be a map of
   build specifications.

   f - is a function of two arguments the first argument will be the
   cursor and the second argument will be a map of optional values
   passed to build. f must return at a minimum an IRender instance,
   this instance may implement other React life cycle protocols.

   cursor - an ICursor instance

   m - a map the following keys are allowed:

     :key       - a keyword that should be used to look up the key used by
                  React itself when rendering sequential things.
     :react-key - an explicit react key
     :fn        - a function to apply to the data at the relative path before
                  invoking f.
     :opts      - a map of options to pass to the component.

   Example:

     (build list-of-gadgets cursor
        {:opts {:event-chan ...
                :narble ...}})
  "
  ([f cursor] (build f cursor nil))
  ([f cursor m]
    (cond
      (nil? m)
      (pure #js {:__om_cursor cursor}
        (fn [this] (allow-reads (f cursor this))))

      :else
      (let [{:keys [key opts]} m
            dataf   (get m :fn)
            cursor' (if-not (nil? dataf) (dataf cursor) cursor)
            rkey    (if-not (nil? key)
                      (get cursor' key)
                      (get m :react-key))]
        (pure #js {:__om_cursor cursor'
                   :__om_index (::index m)
                   :key rkey}
          (if (nil? opts)
            (fn [this] (allow-reads (f cursor' this)))
            (fn [this] (allow-reads (f cursor' this opts)))))))))

(defn build-all
  ([f xs] (build-all f xs nil))
  ([f xs m]
    (into-array
      (map (fn [x i]
             (build f x (assoc m ::index i)))
        xs (range)))))

(defn transact!
  "Given a cursor, a list of keys ks, mutate the tree at the path
   specified by the cursor + the keys by applying f to the specified
   value in the tree. If only given two arguments, assumed no list
   of keys was specified. An Om re-render will be triggered."
  ([cursor f]
    (let [path (.-path cursor)]
      (if (empty? path)
        (swap! (.-state cursor) f)
        (swap! (.-state cursor) update-in path f))))
  ([cursor ks f]
    (swap! (.-state cursor) update-in (into (.-path cursor) ks) f))
  ([cursor ks f a]
    (swap! (.-state cursor) update-in (into (.-path cursor) ks) f a))
  ([cursor ks f a b]
    (swap! (.-state cursor) update-in (into (.-path cursor) ks) f a b))
  ([cursor ks f a b c]
    (swap! (.-state cursor) update-in (into (.-path cursor) ks) f a b c))
  ([cursor ks f a b c d]
    (swap! (.-state cursor) update-in (into (.-path cursor) ks) f a b c d))
  ([cursor ks f a b c d & args]
    (apply swap! (.-state cursor) update-in (into (.-path cursor) ks) f a b c d args)))

(defn read
  "Given a cursor, read its current value. Can take an optional sequence
   of keys ks. Used for interacting with cursors outside of render."
  ([cursor] (read cursor nil))
  ([cursor ks]
    (let [path  (into (.-path cursor) ks)
          state (.-state cursor)
          value @state]
      (if (empty? path)
        (to-cursor value state [])
        (to-cursor (get-in value path) state path)))))

(defn join
  "Given a cursor, get value from the root at the path specified by a
   sequential list of keys ks."
  [cursor ks]
  (let [state (.-state cursor)
        value @state]
    (to-cursor (get-in value ks)
      state (if (vector? ks) ks (into [] ks)))))

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
  (let [props  (.-props owner)
        state  (.-state owner)
        cursor (aget props "__om_cursor")
        path   (.-path cursor)
        pstate (or (aget state "__om_pending_state")
                   (aget state "__om_state"))]
    (aset state "__om_pending_state" (assoc-in pstate ks v))
    ;; invalidate path to component
    (if (empty? path)
      (swap! (.-state cursor) identity)
      (swap! (.-state cursor) update-in path identity))))

(defn get-state
  "Takes a pure owning component and sequential list of keys and returns
   a property if it exists. Will never return pending state values."
  ([owner] (aget (.-state owner) "__om_state"))
  ([owner ks]
    (if (empty? ks)
      (get-state owner)
      (get-in (aget (.-state owner) "__om_state") ks))))
