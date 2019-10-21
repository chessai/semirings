TBA
---
* Add `gcdExt` function

0.5.1: [2019.09.13]
-------------------
* Bump upper bound on containers to 0.7
* Bump upper bound on hashable to 1.4
* Remove redundant constraints from WrappedFractional instances

0.5: [2019.09.05]
-----------------
* Add `Field` typeclass, instances, and functions.
* Add `Euclidean` and `GcdDomain` instances for `()`, `CDouble`, `CFloat`,
  and `Complex`.
* Add `Ring` and `Bits` instances for `WrappedFractional` and `WrappedIntegral`.
* Add `fromInteger` and `fromIntegral` functions for `Ring`.

0.4.2: [2019.06.06]
-------------------
* Add `GcdDomain` and `Euclidean` typeclasses.
* Add `Mod2`, the integers modulo 2, along with its Semiring/Ring/Star
  instances.

0.4.1: [2019.05.04]
-------------------
* Remove unlawful and useless `Ring` instance for `GHC.Natural.Natural`.
* Correct behaviour/docs of Data.Semiring.(^)

0.4: [2019.05.01]
-----------------
* Remove unlawful instances of `Ring` (thanks to @Bodigrim for noticing these)
* Add `fromNatural` to `Semiring` typeclass (thanks @Bodigrim)
* Remove Semiring/Ring instances for [] and Vector. (thanks @Bodigrim)
  These instances are better served by a dedicated polynomial package,
  which @Bodigrim has made at http://hackage.haskell.org/package/poly.
* Add isZero/isOne predicates.

0.3.1.2: [2019.04.02]
---------------------
* Fix build error on windows caused by providing instances
  to POSIX types. Thanks to @Bodigrim and @CarlEdman for
  reporting this.

0.3.1.1: [2019.01.12]
---------------------
* Fix build error caused by disabling building with containers.

0.3.1.0: [2019.01.12]
---------------------
* Add Data.Semiring.Tropical
* Fix build problem on GHC 7.4 caused by introduction of IntSetOf/IntMapOf
* Make sure there are no warnings when building with -Wall, for any GHC

0.3.0.0: [2019.01.01]
---------------------
* Rename the test suite to make `stack` happy.
* Clarified documentation. See #26.
* Simplify implementation of `^`. See #24.
* Add 'GenericSemiring', a newtype wrapper meant to be used with `-XDerivingVia`,
  helping avoid '-XDefaultSignatures'.
* Add newtypes for `IntSet` and `IntMap`.
* Remove `Semiring` and `Ring` instances for `Product` and `Sum`.
* Make `sum` and `product` more efficient for base>=4.7

0.2.1.1: [2018.10.01]
---------------------
* Fixed build on GHC-7.4
* Provide `Semiring` and `Ring` for an arbitrary `Num` via `WrappedNum` newtype.
* Make note of `Semiring` semantics for `Vector` and `[]` in the documentation.
* Require build script to ensure `semirings` builds with GHC-8.4.3 and GHC-8.6.1
* Fixed unlawful behaviour of `[]` `Semiring` instance.
* Improve performance of `^`.

0.2.1.0: [2018.09.26]
---------------------
* Removed use of DefaultSignatures
* Removed free semiring

0.2.0.1: [2018.07.28]
---------------------
* Add instances for `Op`, `Equivalence`, `Comparison`, and `Predicate` from Data.Functor.Contravariant (upcoming base 4.12.0.0)
* docfix for (prod -> product, prod' -> product') change that occured in version 0.2.0.0.

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
