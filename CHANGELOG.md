0.6: [2021-01-07]
-----------------
* Remove hashable flag (only necessary was unordered-containers flag)
* Drop redundant `Eq` constraint on default definition of `coprime`
* Document (lack of guaranteed) rounding behaviour of quotRem
* Fix totally broken Ord instance for Tropical
* Stop depending on integer-gmp

0.5.4: [2020.07.13]
-------------------
* Drop support for GHCs prior to 7.10
* Add default quotRem implementation
* Expose Data.Semiring.Generic.gfromNatural

0.5.3: [2020.02.18]
-------------------
* Fix non-terminating GenericSemiring instances
* Fix incorrect implementation of gtimes' for product types in GSemiring
* Implement GcdDomain.divide explicitly
* Remove redundant imports
* Disambiguate all haddock identifiers

0.5.2: [2019.11.01]
-------------------
* Add `gcdExt` function
* Bump upper bound on base
* Add GcdDomain/Euclidean instances for `Mod2`
* Add GcdDomain/Euclidean instances for {Int|Word}{8|16|32|64}
* Mention `RebindableSyntax` in haddocks

rev: b4334fe06635f106b1f08bac127c1ae259cddae6

0.5.1: [2019.09.13]
-------------------
* Bump upper bound on containers to 0.7
* Bump upper bound on hashable to 1.4
* Remove redundant constraints from WrappedFractional instances
* Add lower bound on semigroups

rev: 7e6f5e312bec5495ce9390664578bfb09d6e3eb9

0.5: [2019.09.05]
-----------------
* Add `Field` typeclass, instances, and functions.
* Add `Euclidean` and `GcdDomain` instances for `()`, `CDouble`, `CFloat`,
  and `Complex`.
* Add `Ring` and `Bits` instances for `WrappedFractional` and `WrappedIntegral`.
* Add `fromInteger` and `fromIntegral` functions for `Ring`.

rev: eb2617d93d354085fe5b706a145108d090dbc027

0.4.2: [2019.06.06]
-------------------
* Add `GcdDomain` and `Euclidean` typeclasses.
* Add `Mod2`, the integers modulo 2, along with its Semiring/Ring/Star
  instances.

rev: b5af2fa403c68a66a3282b2a452b9be1c98e3fd6

0.4.1: [2019.05.04]
-------------------
* Remove unlawful and useless `Ring` instance for `GHC.Natural.Natural`.
* Correct behaviour/docs of Data.Semiring.(^)

rev: d6c42aeea602499e32081e84974910d0fe955db6

0.4: [2019.05.01]
-----------------
* Remove unlawful instances of `Ring` (thanks to @Bodigrim for noticing these)
* Add `fromNatural` to `Semiring` typeclass (thanks @Bodigrim)
* Remove Semiring/Ring instances for [] and Vector. (thanks @Bodigrim)
  These instances are better served by a dedicated polynomial package,
  which @Bodigrim has made at http://hackage.haskell.org/package/poly.
* Add isZero/isOne predicates.

rev: 1285d3e42242db310083fbf78d2e611bccecc63a

0.3.1.2: [2019.04.02]
---------------------
* Fix build error on windows caused by providing instances
  to POSIX types. Thanks to @Bodigrim and @CarlEdman for
  reporting this.

rev: 13d4b3920912f8030b5d47777fb57b6e0dd15c10

0.3.1.1: [2019.01.12]
---------------------
* Fix build error caused by disabling building with containers.

rev: 5f02279613bfcd20c2e9d68f01d669e563540ced

0.3.1.0: [2019.01.12]
---------------------
* Add Data.Semiring.Tropical
* Fix build problem on GHC 7.4 caused by introduction of IntSetOf/IntMapOf
* Make sure there are no warnings when building with -Wall, for any GHC

rev: 68c604250e2cf5688b3c641fd40b66fe7e1d45bf

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

rev: d7d47c3db82a8e85330bb138169b9783eb346f38

0.2.1.1: [2018.10.01]
---------------------
* Fixed build on GHC-7.4
* Provide `Semiring` and `Ring` for an arbitrary `Num` via `WrappedNum` newtype.
* Make note of `Semiring` semantics for `Vector` and `[]` in the documentation.
* Require build script to ensure `semirings` builds with GHC-8.4.3 and GHC-8.6.1
* Fixed unlawful behaviour of `[]` `Semiring` instance.
* Improve performance of `^`.

rev: e9b85d8aa6a238d07a061402f0ba365190eee7aa

0.2.1.0: [2018.09.26]
---------------------
* Removed use of DefaultSignatures
* Removed free semiring

rev: 68e97e82280a50c374f50500a73222a5432cc45e

0.2.0.1: [2018.07.28]
---------------------
* Add instances for `Op`, `Equivalence`, `Comparison`, and `Predicate` from Data.Functor.Contravariant (upcoming base 4.12.0.0)
* docfix for (prod -> product, prod' -> product') change that occured in version 0.2.0.0.

rev: 60869059d2959676877c9661427814b2bafd5d97

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

rev: b985dcf37b919facc2dfbec66ea923ca5427c9f6

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
