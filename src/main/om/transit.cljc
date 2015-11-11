(ns om.transit
  #?(:clj (:refer-clojure :exclude [ref]))
  (:require [cognitect.transit :as t]
   #?(:cljs [com.cognitect.transit :as ct])
            [om.tempid :as tempid #?@(:cljs [:refer [TempId]])])
  #?(:clj (:import [com.cognitect.transit
                    TransitFactory WriteHandler ReadHandler]
                   [om.tempid TempId])))

#?(:cljs
   (deftype TempIdHandler []
     Object
     (tag [_ _] "om/id")
     (rep [_ r] (. r -id))
     (stringRep [_ _] nil)))

#?(:clj
   (deftype TempIdHandler []
     WriteHandler
     (tag [_ _] "om/id")
     (rep [_ r] (. ^TempId r -id))
     (stringRep [_ r] (. ^TempId r -id))
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
          [:handlers "om/id"]
          (fn [id] (tempid/tempid id)))))))

#?(:clj
   (defn reader
     ([in]
      (reader in {}))
     ([in opts]
      (t/reader in :json
        (assoc-in opts
          [:handlers "om/id"]
          (reify
            ReadHandler
            (fromRep [_ id] (TempId. id))))))))

(comment
  ;; cljs
  (t/read (reader) (t/write (writer) (tempid/tempid)))

  ;; clj
  (import '[java.io ByteArrayOutputStream ByteArrayInputStream])

  (def baos (ByteArrayOutputStream. 4096))
  (def w (writer baos))
  (t/write w (TempId. (java.util.UUID/randomUUID)))
  (.toString baos)

  (def in (ByteArrayInputStream. (.toByteArray baos)))
  (def r (reader in))
  (t/read r)
  )