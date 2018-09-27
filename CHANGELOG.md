0.2.1.1: [2018.XX.XX]
---------------------
* Fixed build on GHC-7.4
* Provide `Semiring` and `Ring` for an arbitrary `Num` via `WrappedNum` newtype.

0.2.1.0: [2018.09.26]
---------------------
* Removed use of DefaultSignatures
* Removed free semiring

0.2.0.1: [2018.07.28]
---------------------
* Add instances for `Op`, `Equivalence`, `Comparison`, and `Predicate` from Data.Functor.Contravariant (upcoming base 4.12.0.0)
* docfix for (prod -> product, prod' -> product')

0.2.0.0: [2018.07.23]
---------------------
* Fixed the `Semiring` instances of `Set`, `HashSet`, `Vector`, `Storable Vector`, `Unboxed Vector`.
* Removed the `Semiring` instances of `Seq`, `Alt`, `Endo`.
* Added comprehensive test suite that tests all `Semiring` instances defined in Data.Semiring
* Added Free semiring (Data.Semiring.Free)
* Added newtypes: `Add`, `Mul`
* Bounds for containers: [0.3,0.6] -> [0.5.4,0.6.0.9]
* Add semiring instance for `Proxy`
* names changed: (prod -> product, prod' -> product')
* sum' and product' now use foldl' instead of foldr'

0.1.2: [2018.05.04]
-------------------
* `semirings` now builds back to
  GHC-7.4.1.
* many doc fixes.

0.1.1: [2018.04.20]
-------------------
* Remove unused `coerce-util` dependency.

0.1.0:
------
* Initial version.
