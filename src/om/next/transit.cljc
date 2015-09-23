(ns om.next.transit
  (:require [cognitect.transit :as t]
            [com.cognitect.transit :as ct]
            [om.next.impl.refs :as refs #?@(:cljs [:refer [Ref]])])
  #?(:clj (:import [com.cognitect.transit TransitFactory WriteHandler]
                   [om.next.impl.refs Ref])))

#?(:cljs
   (deftype RefHandler []
     Object
     (tag [_ _] "om/ref")
     (rep [_ r] (ct/tagged "array" #js [(nth r 0) (nth r 1)]))
     (stringRep [_ _] nil)))

#?(:clj
   (deftype RefHandler []
     WriteHandler
     (tag [_ _] "om/ref")
     (rep [_ r] (TransitFactory/taggedValue "array" [(nth r 0) (nth r 1)]))
     (stringRep [_ _] nil)
     (getVerboseHandler [_] nil)))

(defn writer
  ([]
   (writer {}))
  ([opts]
   (t/writer :json
     (assoc-in opts [:handlers Ref] (RefHandler.)))))

(defn reader
  ([]
   (reader {}))
  ([opts]
   (t/reader :json
     (assoc-in opts
       [:handlers "om/ref"]
       (fn [v] (Ref. v))))))

(comment
  (t/write (writer) (Ref. [:root 0]))
  )