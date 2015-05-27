(ns om.next.protocols)

(defprotocol IPull
  (pull [pullable selector context]))

(defprotocol IPush
  (push [pushable tx-data context]))

(defprotocol IPullAsync
  (pull-async [pullable selector context cb]))

(defprotocol IPushAsync
  (push-async [pushable tx-data context cb]))

(defprotocol IStore
  (commit [store tx-data context]))

(defprotocol IComponentIndex
  (index-component [this component])
  (drop-component [this component]))
