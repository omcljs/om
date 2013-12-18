(ns om.core)

(defmacro pure [obj children]
  `(om.core/Pure. ~obj ~children))

(defmacro component [& body]
  `(reify
     om.core/IRender
     (~'render [this# _#]
       ~@body)))
