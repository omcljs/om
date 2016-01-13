(ns om.tempid
  #?(:clj (:import [java.io Writer])))

;; =============================================================================
;; ClojureScript

#?(:cljs
   (deftype TempId [^:mutable id ^:mutable __hash]
     Object
     (toString [this]
       (pr-str this))
     IEquiv
     (-equiv [this other]
       (and (instance? TempId other)
            (= (. this -id) (. other -id))))
     IHash
     (-hash [this]
       (when (nil? __hash)
         (set! __hash (hash id)))
       __hash)
     IPrintWithWriter
     (-pr-writer [_ writer _]
       (write-all writer "#om/id[\"" id "\"]"))))

#?(:cljs
   (defn tempid
     ([]
      (tempid (random-uuid)))
     ([id]
      (TempId. id nil))))

;; =============================================================================
;; Clojure

#?(:clj
   (defrecord TempId [id]
     Object
     (toString [this]
       (pr-str this))))

#?(:clj
   (defmethod print-method TempId [^TempId x ^Writer writer]
     (.write writer (str "#om/id[\"" (.id x) "\"]"))))

#?(:clj
   (defn tempid [uuid]
     (TempId. uuid)))

(defn tempid?
  #?(:cljs {:tag boolean})
  [x]
  (instance? TempId x))
