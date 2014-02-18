(ns om.core)

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
              "om.core/transact!, om.core/update!, and cljs.core/deref operations allowed")))))

(defmacro tag [pure t]
  `(let [pure# ~pure]
     (set! (.-constructor pure#) (goog/getUid ~t))
     pure#))
