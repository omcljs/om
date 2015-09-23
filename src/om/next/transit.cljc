(ns om.next.transit
  #?(:clj (:refer-clojure :exclude [ref]))
  (:require [cognitect.transit :as t]
            #?(:cljs [com.cognitect.transit :as ct])
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

#?(:cljs
   (defn writer
     ([]
      (writer {}))
     ([opts]
      (t/writer :json
        (assoc-in opts [:handlers Ref] (RefHandler.))))))

#?(:clj
   (defn writer
     ([out]
      (writer out {}))
     ([out opts]
      (t/writer out :json
        (assoc-in opts [:handlers Ref] (RefHandler.))))))

#?(:cljs
   (defn reader
     ([]
      (reader {}))
     ([opts]
      (t/reader :json
        (assoc-in opts
          [:handlers "om/ref"]
          (fn [v] (Ref. v)))))))

#?(:clj
   (defn reader
     ([in]
      (reader in {}))
     ([in opts]
      (t/reader in :json
        (assoc-in opts
          [:handlers "om/ref"]
          (fn [v] (Ref. v)))))))

(comment
  ;; cljs
  (t/write (writer) (Ref. [:root 0]))

  ;; clj
  (import '[java.io ByteArrayOutputStream])
  (def baos (ByteArrayOutputStream. 4096))
  (def w (writer baos))
  (t/write w (Ref. [:root 0]))
  (.toString baos)
  )