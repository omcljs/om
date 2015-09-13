(ns om.next.protocols)

(defprotocol IIndexer
  (indexes [this])
  (index-component! [this component])
  (drop-component! [this component])
  (props-for [this component]))

(defprotocol IReconciler
  (basis-t [this])
  (state [this])
  (add-root! [reconciler target root-class options])
  (remove-root! [reconciler target])
  (commit! [queue tx-type tx-data context])
  (schedule! [reconciler])
  (reconcile! [reconciler]))
