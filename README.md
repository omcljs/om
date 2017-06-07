# Om

A [ClojureScript](http://github.com/clojure/clojurescript) UI framework and
client/server architecture over [Facebook's
React](http://facebook.github.io/react/).

Om UIs are out of the box snapshotable and undoable and these operations have 
no implementation complexity and little overhead.

Om borrows ideas liberally from [Facebook's
Relay](https://facebook.github.io/relay/) and [Netflix's
Falcor](http://netflix.github.io/falcor/) with a dash of inspiration from
[Datomic pull syntax](http://docs.datomic.com/pull.html) to avoid the typical 
incidental complexity that arises from client/server state management.

## Example

```clojure
(ns example
  (:require [om.dom :as dom]
            [om.next :as om]))

(defui Widget
  om/IRender
  (render [this]
    (dom/h1 nil (:text data)))

(def widget (om/factory Widget)

(om/root widget {:text "Hello world!"}
  {:target (. js/document (getElementById "my-app"))})
```

## Tutorials

There is an Quick Start tutorial that will introduce you to the core
concepts of Om
[here](https://github.com/omcljs/om/wiki/Quick-Start-%28om.next%29).

## Documentation

There is documentation [here](https://github.com/omcljs/om/wiki/Documentation-%28om.next%29)

## Contributing

Please contact me via email to request an electronic Contributor
Agreement. Once your electronic CA has been signed and returned to me
I will accept pull requests.

## Community

If you are looking for help please get in touch either on the 
[clojurians.slack.com **#om** channel](http://clojurians.net) or the 
[om-cljs Google Group](https://groups.google.com/d/forum/om-cljs).  

## References

* [Worlds: Controlling the Scope of Side Effects](http://www.vpri.org/pdf/tr2011001_final_worlds.pdf)
* [A Functional I/O System](http://www.ccs.neu.edu/racket/pubs/icfp09-fffk.pdf)
* [Directness and Liveness in the Morphic User Interface Construction Environment](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.103.600&rep=rep1&type=pdf)
* [Learnable Programming](http://worrydream.com/LearnableProgramming/)
* [Relay](https://facebook.github.io/relay/)
* [Falcor](http://netflix.github.io/falcor/)
* [GraphQL](http://graphql.org)
* [Datomic pull syntax](http://docs.datomic.com/pull.html)

## Copyright and license

Copyright © 2013-2017 David Nolen

Licensed under the EPL (see the file epl.html).
