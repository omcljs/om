(ns om.next.protocols)

(defprotocol IIndexer
  (indexes [this])
  (index-root [this x])
  (index-component! [this component])
  (drop-component! [this component])
  (ref-for [this component])
  (key->components [this k]))

(defprotocol IReconciler
  (basis-t [this])
  (add-root! [reconciler root-class target options])
  (remove-root! [reconciler target])
  (schedule-render! [reconciler])
  (schedule-send! [reconciler])
  (queue! [reconciler ks])
  (queue-send! [reconciler expr])
  (reindex! [reconciler])
  (reconcile! [reconciler])
  (send! [reconciler]))
