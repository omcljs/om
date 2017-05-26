# Om

A [ClojureScript](http://github.com/clojure/clojurescript) UI framework and
client/server architecture over [Facebook's
React](http://facebook.github.io/react/).

Om allows users to represent their UIs simply as
[EDN](http://github.com/edn-format/edn). Om UIs are out of the box snapshotable
and undoable and these operations have no implementation complexity and little
overhead.

Om borrows ideas liberally from [Facebook's
Relay](https://facebook.github.io/relay/) and [Netflix's
Falcor](http://netflix.github.io/falcor/) with a dash of inspiration from
[Datomic pull syntax](http://docs.datomic.com/pull.html) to remove incidental
complexity from client/server state management.

## Example

```clojure
(ns example
  (:require [om.dom :as dom]
            [om.next :as om]))

(defn widget [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil (:text data)))))

(om/root widget {:text "Hello world!"}
  {:target (. js/document (getElementById "my-app"))})
```

## Tutorials

There is an in-depth tutorial that will introduce you to the core
concepts of Om
[here](http://github.com/swannodette/om/wiki/Basic-Tutorial) and a
real-world integration example
[here](http://github.com/swannodette/om/wiki/Intermediate-Tutorial). The
community maintained [om-cookbook](https://github.com/omcljs/om-cookbook)
covers many common idioms and patterns.

## Documentation

There is documentation [here](http://github.com/swannodette/om/wiki/Documentation).

There is also a
[conceptual overview](http://github.com/swannodette/om/wiki/Conceptual-overview)
that we recommend reading as there are some design choices in Om that
make it quite different from other client side solutions and even
React itself.

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

## Copyright and license

Copyright © 2013-2017 David Nolen

Licensed under the EPL (see the file epl.html).
