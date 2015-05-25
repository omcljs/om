(ns om.next.protocols)

(defprotocol IStore
  (-query [this qs]))

(defprotocol IRemoteStore
  (-remote-query [this qs cb]))