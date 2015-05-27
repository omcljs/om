(ns om.next.protocols)

(defprotocol IPull
  (pull [pullable selector context]))

(defprotocol IPush
  (push [pushable entity context]))

(defprotocol IPullAsync
  (pull-async [pullable selector context cb]))

(defprotocol IPushAsync
  (push-async [pushable entity context cb]))

(defprotocol IStore
  (commit [store component entity]))