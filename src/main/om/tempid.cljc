(ns om.next.tempid)

#?(:cljs
   (defonce tempids
     (atom {:system {}})))

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
     (-pr-writer [this writer opts]
       (if-not frozen
         (write-all writer "#om/id[?" id "]")
         (write-all writer "#om/id[" id "]")))))

(defn tempid []
  (let [uuid (random-uuid)
        ret  (TempId. uuid false)]
    (swap! tempids update-in [:system] assoc uuid ret)
    ret))
