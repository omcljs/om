(ns om.next.protocols)

(defprotocol IStore)

(defprotocol IPull
  (pull [pullable selector context]))

(defprotocol IPush
  (push [pushable tx-data context]))

(defprotocol IPullAsync
  (pull-async [pullable selector context cb]))

(defprotocol IPushAsync
  (push-async [pushable tx-data context cb]))

(defprotocol IComponentIndex
  (index-component! [this component])
  (drop-component! [this component]))

(defprotocol ICommitQueue
  (commit! [queue tx-type tx-data context]))

(defprotocol IReconciler
  (basis-t [this])
  (store [this])
  (indexes [this])
  (props-for [this component])
  (add-root! [reconciler target root-class options])
  (remove-root! [reconciler target])
  (schedule! [reconciler])
  (reconcile! [reconciler]))
