(ns om.tempid
  #?(:clj (:import [java.io Writer])))

#?(:cljs
   (defonce tempids
     (atom {:system {}})))

;; =============================================================================
;; ClojureScript

#?(:cljs
   (deftype TempId [^:mutable id ^:mutable frozen]
     Object
     (toString [this]
       (pr-str this))
     (freezeID [this real-id]
       (if-not frozen
         (set! id real-id)
         (throw (js/Error. "TempId already frozen"))))
     IEquiv
     (-equiv [this other]
       (and (instance? TempId other)
         (= (. this -id) (. other -id))))
     IPrintWithWriter
     (-pr-writer [_ writer _]
       (if-not frozen
         (write-all writer "#om/id[?" id "]")
         (write-all writer "#om/id[" id "]")))))

#?(:cljs
   (defn tempid
     ([]
       (tempid (random-uuid)))
     ([id]
       (if-let [tid (contains? @tempids id)]
         tid
         (let [new-tid (TempId. id false)]
           (swap! tempids assoc id new-tid)
           new-tid)))))

;; =============================================================================
;; Clojure

#?(:clj
   (defrecord TempId [id]
     Object
     (toString [this]
       (pr-str this))))

#?(:clj
   (defmethod print-method TempId [^TempId x ^Writer writer]
     (.write writer (str "#om/id[?" (.id x) "]"))))

#?(:clj
   (defn tempid [uuid]
     (TempId. uuid)))
