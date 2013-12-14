# om

A ClojureScript interface to Facebook's React.

## Example

```clj
(ns example
  (:require [om.core :as om]
            [om.dom :as dom :include-macros true]))

(defn widget [data]
  (dom/component
    (dom/div nil "Hello world!"))))

(om/root {} widget js/document.body)
```
