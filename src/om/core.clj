(ns om.core)

(defmacro pure [obj children]
  `(om.core/Pure. ~obj ~children))

(defmacro component
  "Sugar over reify for quickly putting together components that
   only need to implement om.core/IRender and don't need access to
   the owner argument."
  [& body]
  `(reify
     om.core/IRender
     (~'render [this#]
       ~@body)))

(defmacro allow-reads [& body]
  `(binding [om.core/*read-enabled* true]
    ~@body))

(defmacro check [& body]
  `(if om.core/*read-enabled*
     (do
       ~@body)
     (throw
       (js/Error.
         (str "Cannot manipulate cursor outside of render phase, only "
              "om.core/transact!, om.core/update!, and om.core/read operations allowed")))))

(defmacro cursor-check [cursor m & body]
  `(if (or (:om.core/raw m) (om.core/cursor? ~cursor))
     (do ~@body)
     (throw (js/Error. (str "Cannot build Om component from non-cursor " ~cursor)))))

(defmacro safe-transact! [cursor korks f & args]
  `(om.core/allow-reads
     (let [path#  (om.core/-path ~cursor)
           state# (om.core/-state ~cursor)]
       (if-not (sequential? ~korks)
         (swap! state# update-in (conj path# ~korks) ~f ~@args)
         (swap! state# update-in (into path# ~korks) ~f ~@args)))))

(defmacro safe-update! [cursor f & args]
  `(om.core/allow-reads
     (let [path#  (om.core/-path ~cursor)
           state# (om.core/-state ~cursor)]
       (if (empty? path#)
         (swap! state# #(~f % ~@args))
         (swap! state# update-in path# ~f ~@args)))))

(defmacro tag [pure t]
  `(let [pure# ~pure]
     (set! (.-constructor pure#) (goog/getUid ~t))
     pure#))
