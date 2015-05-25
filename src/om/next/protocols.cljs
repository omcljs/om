(ns om.next.protocols)

(defprotocol IStore
  (-query [this q]))

(defprotocol IRemoteStore
  (-remote-query [this q]))