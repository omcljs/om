(ns om.next.transit
  (:require [cognitect.transit :as t]
            [om.next.impl.refs :as refs #?@(:cljs [:refer [Ref]])])
  #?(:clj (:import [com.cognitect.transit TransitFactory WriteHandler]
                   [om.next.impl.refs Ref])))

#?(:cljs
   (deftype RefHandler []
     Object
     (tag [_ _] "om/ref")
     (rep [_ r] (t/tagged "array" [(nth r 0) (nth r 1)]))
     (stringRep [_ _] nil)))

#?(:clj
   (deftype RefHandler []
     WriteHandler
     (tag [_ _] "om/ref")
     (rep [_ r] (TransitFactory/taggedValue "array" [(nth r 0) (nth r 1)]))
     (stringRep [_ _] nil)
     (getVerboseHandler [_] nil)))

(defn writer [opts]
  (t/writer :json
    (assoc opts
      [:handlers "om/ref"]
      (fn [v] (Ref. v)))))

(defn reader [opts]
  (t/reader :json
    (assoc-in opts [:handlers Ref] RefHandler)))