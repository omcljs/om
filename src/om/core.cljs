(ns om.core
  (:require-macros
    [om.core :refer [pure component check allow-reads safe-update! safe-transact!]])
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

(defn ^:private children [node]
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

                   (not (nil? (aget (.-state this) "__om_pending_state")))
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

(defprotocol ICursor
  (-path [cursor]))

(defn path [cursor]
  (-path cursor))

(defn cursor? [x]
  (satisfies? ICursor x))

(deftype MapCursor [value state path]
  ICursor
  (-path [_] (check path))
  ICloneable
  (-clone [_]
    (MapCursor. value state path))
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
  ISequential
  ICursor
  (-path [_] (check path))
  ICloneable
  (-clone [_]
    (VectorCursor. value state path))
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
                (let [value  @state
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

   f - is a function of up to three arguments. The first argument will
   be the cursor and the second argument will be the owning pure node.
   If a map of options m is passed in this will be the third
   argument. f must return at a minimum an IRender instance, this
   instance may implement other React life cycle protocols.

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
  "Given a cursor, an optional list of keys ks, mutate the tree at the
   path specified by the cursor + the optional keys by applying f to the
   specified value in the tree. An Om re-render will be triggered."
  ([cursor f]
    (let [state (.-state cursor)
          path  (.-path cursor)]
      (if (empty? path)
        (swap! state f)
        (swap! state update-in path f))))
  ([cursor korks f]
    (safe-transact! cursor korks f))
  ([cursor korks f a]
    (safe-transact! cursor korks f a))
  ([cursor korks f a b]
    (safe-transact! cursor korks f a b))
  ([cursor korks f a b c]
    (safe-transact! cursor korks f a b c))
  ([cursor korks f a b c d]
    (safe-transact! cursor korks f a b c d))
  ([cursor korks f a b c d & args]
    (let [state (.-state cursor)
          path  (.-path cursor)]
      (if-not (sequential? korks)
        (apply swap! state update-in (conj path korks) f a b c d args)
        (apply swap! state update-in (into path korks) f a b c d args)))))

(defn update!
  "Like transact! but no list of keys given. An Om re-render
   will be triggered."
  ([cursor f]
    (safe-update! cursor f))
  ([cursor f a]
    (safe-update! cursor f a))
  ([cursor f a b]
    (safe-update! cursor f a b))
  ([cursor f a b c]
    (safe-update! cursor f a b c))
  ([cursor f a b c d]
    (safe-update! cursor f a b c d))
  ([cursor f a b c d & args]
    (let [path  (.-path cursor)
          state (.-state cursor)]
      (if (empty? path)
        (swap! state #(apply f % a b c d args))
        (apply swap! state update-in path f a b c d args)))))

(defn read
  "Given a cursor and a function f, read its current value. f will be
   passed a cursor which can only be read in the scope of f. Can take
   an optional sequence of keys ks. Used for interacting with cursors
   outside of render phase."
  ([cursor f] (read cursor () f))
  ([cursor korks f]
    (let [path  (if-not (sequential? korks)
                  (conj (.-path cursor) korks)
                  (into (.-path cursor) korks))
          state (.-state cursor)
          value @state]
      (if (empty? path)
        (allow-reads (f (to-cursor value state [])))
        (allow-reads (f (to-cursor (get-in value path) state path)))))))

(defn join
  "Given a cursor, get value from the root at the path specified by a
   sequential list of keys ks."
  [cursor korks]
  (let [state (.-state cursor)
        value @state]
    (if-not (sequential? korks)
      (to-cursor (get value korks) state [korks])
      (to-cursor (get-in value korks) state
        (if (vector? korks) korks (into [] korks))))))

(defn get-node
  "A helper function to get at React refs. Given a owning pure node
  extract the ref specified by name."
  [owner name]
  (when-let [refs (.-refs owner)]
    (.getDOMNode (aget refs name))))

(defn set-state!
  "Takes a pure owning component, a sequential list of keys and value and
   sets the state of the component. Conceptually analagous to React
   setState. Will schedule an Om re-render."
  [owner korks v]
  (let [props  (.-props owner)
        state  (.-state owner)
        cursor (aget props "__om_cursor")
        path   (.-path cursor)
        pstate (or (aget state "__om_pending_state")
                   (aget state "__om_state"))]
    (if-not (sequential? korks)
      (aset state "__om_pending_state" (assoc pstate korks v))
      (aset state "__om_pending_state" (assoc-in pstate korks v)))
    ;; invalidate path to component
    (if (empty? path)
      (swap! (.-state cursor) clone)
      (swap! (.-state cursor) update-in path clone))))

(defn get-state
  "Takes a pure owning component and sequential list of keys and returns
   a property if it exists. Will never return pending state values."
  ([owner] (aget (.-state owner) "__om_state"))
  ([owner korks]
    (cond
      (not (sequential? korks))
      (get (aget (.-state owner) "__om_state") korks)

      (empty? korks)
      (get-state owner)

      :else
      (get-in (aget (.-state owner) "__om_state") korks))))

(defn bind
  "Convenience function for creating event handlers on cursors. Takes
   a function f which should receive the event as the first argument,
   the cursor as the second argument, and any number of optional
   arguments beyond that. Inside of f the cursor will be readable."
  ([f cursor]
    (fn [e]
      (read cursor
        (fn [cursor]
          (f e cursor)))))
  ([f cursor a]
    (fn [e]
      (read cursor
        (fn [cursor]
          (f e cursor a)))))
  ([f cursor a b]
    (fn [e]
      (read cursor
        (fn [cursor]
          (f e cursor a b)))))
  ([f cursor a b c]
    (fn [e]
      (read cursor
        (fn [cursor]
          (f e cursor a b c)))))
  ([f cursor a b c d]
    (fn [e]
      (read cursor
        (fn [cursor]
          (f e cursor a b c d)))))
  ([f cursor a b c d & args]
    (fn [e]
      (read cursor
        (fn [cursor]
          (apply f e cursor a b c d args))))))
