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
