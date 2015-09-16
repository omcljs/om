(ns om.next.protocols)

(defprotocol IIndexer
  (indexes [this])
  (index-root [this root-class])
  (index-component! [this component])
  (drop-component! [this component])
  (ref-for [this component])
  (key->components [this k]))

(defprotocol IReconciler
  (basis-t [this])
  (add-root! [reconciler target root-class options])
  (remove-root! [reconciler target])
  (schedule-send! [reconciler expr])
  (queue! [queue k-or-ks])
  (schedule-render! [reconciler])
  (reconcile! [reconciler]))
