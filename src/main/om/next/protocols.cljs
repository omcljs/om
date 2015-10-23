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
  (schedule-sends! [reconciler])
  (queue! [reconciler ks])
  (queue-sends! [reconciler sends])
  (reindex! [reconciler])
  (reconcile! [reconciler])
  (send! [reconciler]))
