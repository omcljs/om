(ns om.transit
  #?(:clj (:refer-clojure :exclude [ref]))
  (:require [cognitect.transit :as t]
    #?(:cljs [com.cognitect.transit :as ct])
            [om.tempid :as tempid #?@(:cljs [:refer [TempId]])])
  #?(:clj (:import [com.cognitect.transit TransitFactory WriteHandler]
                   [om.tempid TempId])))

#?(:cljs
   (deftype TempIdHandler []
     Object
     (tag [_ _] "om/id")
     (rep [_ r] (ct/tagged "array" #js [(nth r 0) (nth r 1)]))
     (stringRep [_ _] nil)))

#?(:clj
   (deftype TempIdHandler []
     WriteHandler
     (tag [_ _] "om/id")
     (rep [_ r] (TransitFactory/taggedValue "array" [(nth r 0) (nth r 1)]))
     (stringRep [_ _] nil)
     (getVerboseHandler [_] nil)))

#?(:cljs
   (defn writer
     ([]
      (writer {}))
     ([opts]
      (t/writer :json
        (assoc-in opts [:handlers TempId] (TempIdHandler.))))))

#?(:clj
   (defn writer
     ([out]
      (writer out {}))
     ([out opts]
      (t/writer out :json
        (assoc-in opts [:handlers TempId] (TempIdHandler.))))))

#?(:cljs
   (defn reader
     ([]
      (reader {}))
     ([opts]
      (t/reader :json
        (assoc-in opts
          [:handlers "om/ref"]
          (fn [v] (TempId. v)))))))

#?(:clj
   (defn reader
     ([in]
      (reader in {}))
     ([in opts]
      (t/reader in :json
        (assoc-in opts
          [:handlers "om/ref"]
          (fn [v] (TempId. v)))))))

(comment
  ;; cljs
  (t/write (writer) (TempId. (java.util.UUID/randomUUID)))

  ;; clj
  (import '[java.io ByteArrayOutputStream])
  (def baos (ByteArrayOutputStream. 4096))
  (def w (writer baos))
  (t/write w (Ref. [:root 0]))
  (.toString baos)
  )