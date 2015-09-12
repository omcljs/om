(ns om.next.protocols)

(defprotocol IComponentIndex
  (index-component! [this component])
  (drop-component! [this component]))

(defprotocol ICommitQueue
  (commit! [queue tx-type tx-data context]))

(defprotocol IReconciler
  (basis-t [this])
  (state [this])
  (indexes [this])
  (props-for [this component])
  (add-root! [reconciler target root-class options])
  (remove-root! [reconciler target])
  (schedule! [reconciler])
  (reconcile! [reconciler]))
