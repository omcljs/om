(ns reactjs.macros)

(defmacro pure [value children]
  `(reactjs.core/Pure. (js-obj "value" value) (fn [] children)))
