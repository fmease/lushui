* fail: access private binding (which is not marked with any `@(public self)` or similar)
  * accessing with `use`
  * accessing in value or data binding
* pass: accessing constructors from data type
  * defined in same module
  * defined in same module and marked `@opaque`
  * marked `@public` and from a different module
* fail: accessing constructors from `@opaque` data type from different module
  * data type is `@public`
  * data type is private
* fail: restricted exposure, reach is undefined
* fail: restricted exposure, reach is not an ancestor
  * it is the binding itself (needs to be a module)
  * it is a sibling module
    * is public
    * is private
  * it is a submodule
* fail: restricted exposure, reach is not a module
* pass: some restricted exposure stuff, not too weird
* pass: some weird out-of-order deeper use chain stuff with a lot of restricted exposure
* pass: re-exporting stuff where the privacy of the reference
  * matches
  * is bigger
* fail: re-exporting private stuff
