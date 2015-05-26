(ns om.next.protocols)

(defprotocol IStore
  (-run-query [this q]))

(defprotocol IRemoteStore
  (-run-remote-query [this q cb]))