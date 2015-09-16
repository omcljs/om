(ns om.next.protocols)

(defprotocol IIndexer
  (indexes [this])
  (index-root [this root-class])
  (index-component! [this component])
  (drop-component! [this component])
  (ref-for [this component]))

(defprotocol IReconciler
  (basis-t [this])
  (app-state [this])
  (indexer [this])
  (parser [this])
  (add-root! [reconciler target root-class options])
  (remove-root! [reconciler target])
  (queue! [queue k-or-ks])
  (schedule! [reconciler])
  (reconcile! [reconciler]))
