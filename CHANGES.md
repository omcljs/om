## 0.8.8

### Changes
* Leverage 0.0-2755 macro usage enhancements

## 0.8.7

### Changes
* Rely on newer cljsjs.react artifact

## 0.8.6

### Changes
* Rely on cljsjs.react artifact

## 0.8.5

### Fixes
* Fix subtle rendering bug in code set-state! queue logic

## 0.8.4

### Changes
* Bump dependencies

## 0.8.3

### Changes
* Bump dependencies

## 0.8.2

### Changes
* Bump dependencies

## 0.8.1

### Changes
* Proper fix to OM-300
* Bump React JAR dep to 0.12.2.2

## 0.8.0

### Changes
* OM-307: get-props extended to have a two arity form (korks)
* OM-306: dissoc :raf

## 0.8.0-rc1

### Changes
* relax transact! precondition, only require ITransact instance

### Fixes
* OM-301: Deprecation warning for om.dom/input
* OM-300: do not preserve component local state entry after unmount

## 0.8.0-beta5

### Changes
* Depend on 0.12.2.1 React JAR with new externs

## 0.8.0-beta4

### Changes
* Add low-level namespace om.impl
* update to React 0.12.2

### Fixes
* OM-190: add :key-fn option to `om.core/build`
* OM-253: overrideable rAF
* OM-294: IKVReduce for MapCursor & IndexedCursor
* OM-296: `no-local-merge-pending-state` was not setting `:previous-state`

## 0.8.0-beta3

### Fixes
* OM-290: component? precondition unexpectedly failing

## 0.8.0-beta2

### Enhancements
* Preconditions on most of the public api to support earlier failures
* Make `render-all` public

## 0.8.0-beta1

### Enhancements
* Improved multimethod support, mount/unmount life-cycle methods invoked
as expected

## 0.8.0-alpha2

### Enhancements
* OM-260: remove cursor consistency checks

### Fixes
* OM-276: bad pending state for no local state
* OM-262: input behavior regression
* OM-270: incorrect no-local state behavior
* OM-274: incorrect -lookup behavior for IndexedCursor
* OM-267: bad logic for not found case in MapCusor -lookup implementation
* OM-271: typo, parent not passed to ref sub-cursor

## 0.8.0-alpha1

### Enhancements
* reference cursors
* om.core/commit!, like om.core/transact! but will not trigger a re-render
* add marquee tag
* add om.core/mounted?
* experimental support to write component local state into global state

### Changes
* React 0.11.2

### Fixes
* om.core/root now properly returns mounted component
* default shouldComponentUpdate now compares state

## 0.7.3

### Changes
* OM-243: fix regression in supporting components built from non-cursors

## 0.7.2

### Changes
* OM-239: update components if cursor path changes

## 0.7.1

### Changes
* OM-133: validate Om component fn return values
* OM-134: add om.core/set-state-nr! and om.core/update-state-nr!, they do not refresh (experimental)
* OM-162: extend default cursor to IEmptyableCollection (experimental)
* OM-180: add om.core/detach-root to remove Om render loop
* OM-214: Cursors should implement IHash

## 0.7.0

### Changes
* BREAKING: :ctor options deprecated for :descriptor see docs
* depend on ClojureScript 0.0-2277
* depend on React 0.11.1

Fixes
###
* OM-170: :tx-listen registration issues

## 0.6.5

### Changes
* depend on Clojure 1.6.0 and ClojureScript 0.0-2268

## 0.6.4

### Changes
* depend on 0.9.0.2 React CLJS

## 0.6.3

### Enhancements
* added ellipse tag

### Bug Fixes
* OM-179: prevent forceUpdate() if component not mounted
* OM-175: error on render-to-str with set-state!

## 0.6.2

### Bug Fixes
* Stale code around component local state

## 0.6.1

### Enhancements
* Pure shouldComponentUpdate logic now uses equiv instead of identical?
* add om.core/rendering? predicate to detect React render phase
* more sensible handling of component local state w/o internal graft,
  we now rely on React's forceUpdate
* add ICursorDerive protocol

### Bug Fixes
* OM-155: multiroot and & :tx-listen incompatible
* OM-152: pass ::index to :fn if available
* OM-150: add missing SVG tags

## 0.6.0

### Breaking Changes
* OM-152: eliminate om.core/graft

### Changes
* added ICursorDerive protocol
* document :ctor option to om.core/build
* add om.core/rendering? predicate
* pass ::index to :fn if available in om.core/build

### Fixes
* OM-150: missing SVG tags
* OM-155: multiroot & :tx-listen incompatible

## 0.5.3

### Changes
* added pure-methods for reuse
* added specify-state-methods! for reuse
* can explicitly give a component an id via :om.core/id with :init-state

## 0.5.2

### Enhancements
* OM-127: om.core/refresh!
* OM-28: om.core/update-state!
* protocols for local state
* Pure implementation methods now provided in immutable map

## 0.5.1

### Changes
* Requires ClojureScript 0.0-2173
* IOmSwap protocol for app state representations besides standard atom
* :instrument option for om.core/root
* Add IDisplayName to support React Chrome Dev Tools
* om.core/build takes :ctor option to provide React backing component
  class besides om.core/Pure

### Fixes
* OM-122: IWillReceiveProps protocol

## 0.5.0

### Changes
* Upgrade to React 0.9.0

## 0.5.0-rc1

### Breaking Changes
* IDidMount, IDidUpdate no longer take node to match React 0.9.0-rc1

### Enhancements
* om.dom/render-to-str added
* OM-72: :path option for om.core/root

## 0.4.2

### Enhancements
* OM-112: transact! special cases when korks is nil

### Bug fixes
* OM-114: om.core/update! doesn't pass tag

## 0.4.1

### Bug fixes
* OM-110: transact! broken for empty path

## 0.4.0

### Breaking Changes
* `om.core/root` signature changed to be more like `om.core/build`
* `om.core/update!` no longer takes keys, or function, or function args
* `om.core/transact!` no longer takes arguments after function

### Enhancements
* `om.core/transact!` now takes optional 4th argument, a tag
* `om.core/update!` now takes optional 4th argument, a tag
* `om.core/root` now supports `:tx-listen` option to observe all transactions

### Bug fixes
* `om.core/shared` was brittle, now works without cursors
