{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
--   A tropical semiring is an extension of another totally ordered
--   semiring with the operations of minimum or maximum as addition.
--   The extended semiring is given positive or negative infinity as
--   its 'zero' element, so that the following hold:
--
-- @
--'plus' 'Infinity' y = y
--'plus' x 'Infinity' = x@
--
--
--   i.e., In the max-plus tropical semiring (where 'plus' is 'max'),
--   'Infinity' unifies with the typical interpretation of negative infinity,
--   and thus it is the identity for the maximum, and in the min-plus tropical
--   semiring (where 'plus' is 'min'), 'Infinity' unifies with the typical
--   interpretation of positive infinity, and thus it is the identity for the minimum.
--
-----------------------------------------------------------------------------

module Data.Semiring.Tropical
  ( Tropical(Infinity,Tropical)
  , Extrema(Minima,Maxima)
  , Extremum(extremum)
  , EProxy(EProxy)
  ) where

#if MIN_VERSION_base(4,7,0)
import Data.Data (Data)
#endif
import Data.Semiring (Semiring(..))
import Data.Star (Star(..))
#if MIN_VERSION_base(4,7,0)
import Data.Typeable (Typeable)
#endif

-- done for haddocks, to make sure -Wall works
import qualified Data.Monoid as Monoid

-- | On older GHCs, 'Data.Proxy.Proxy' is not polykinded, so we provide our own proxy type for 'Extrema'.
--   This turns out not to be a problem, since 'Extremum' is a closed typeclass.
data EProxy (e :: Extrema) = EProxy

-- | A datatype to be used at the kind-level. Its only
--   purpose is to decide the ordering for the tropical
--   semiring in a type-safe way.
data Extrema = Minima | Maxima

-- | The 'Extremum' typeclass exists for us to match on
--   the kind-level 'Extrema', so that we can recover
--   which ordering to use in the `Semiring` instance
--   for 'Tropical`.
class Extremum (e :: Extrema) where
  -- unfortunately this has to take a `Proxy` because
  -- we don't have visual type applications before GHC 8.0
  extremum :: EProxy e -> Extrema

instance Extremum 'Minima where
  extremum _ = Minima
  {-# INLINE extremum #-} -- just to be safe

instance Extremum 'Maxima where
  extremum _ = Maxima
  {-# INLINE extremum #-} -- just to be safe

-- | The tropical semiring.
--
--   @'Tropical' ''Minima' a@ is equivalent to the semiring
--   \( (a \cup \{+\infty\}, \oplus, \otimes) \), where \( x \oplus y = min\{x,y\}\) and \(x \otimes y = x + y\).
--
--   @'Tropical' ''Maxima' a@ is equivalent to the semiring
--   \( (a \cup \{-\infty\}, \oplus, \otimes) \), where \( x \oplus y = max\{x,y\}\) and \(x \otimes y = x + y\).
--
-- In literature, the 'Semiring' instance of the 'Tropical' semiring lifts
-- the underlying semiring's additive structure. One might ask why this lifting doesn't
-- instead witness a 'Monoid.Monoid', since we only lift 'zero' and 'plus' - the reason is
-- that usually the additive structure of a semiring is monotonic, i.e.
-- @a '+' ('min' b c) == 'min' (a '+' b) (a '+' c)@, but in general this is not true.
-- For example, lifting 'Monoid.Product' 'Word' into 'Tropical' is lawful,
-- but 'Monoid.Product' 'Int' is not, lacking distributivity: @(-1) '*' ('min' 0 1) '/=' 'min' ((-1) '*' 0) ((-1) '*' 1)@.
-- So, we deviate from literature and instead
-- witness the lifting of a 'Monoid.Monoid', so the user must take care to ensure
-- that their implementation of 'mappend' is monotonic.
data Tropical (e :: Extrema) a
  = Infinity
  | Tropical a
  deriving
    ( Eq
    , Show
    , Read
#if MIN_VERSION_base(4,7,0)
    , Typeable
    , Data
#endif
    )

instance forall e a. (Ord a, Extremum e) => Ord (Tropical e a) where
  compare Infinity Infinity         = EQ
  compare Infinity _                = case extremum (EProxy :: EProxy e) of
    Minima -> GT
    Maxima -> LT
  compare _ Infinity                = case extremum (EProxy :: EProxy e) of
    Minima -> LT
    Maxima -> GT
  compare (Tropical x) (Tropical y) = compare x y

instance forall e a. (Ord a, Monoid.Monoid a, Extremum e) => Semiring (Tropical e a) where
  zero = Infinity
  one  = Tropical Monoid.mempty
  plus Infinity y = y
  plus x Infinity = x
  plus (Tropical x) (Tropical y) = Tropical
    ( case extremum (EProxy :: EProxy e) of
        Minima -> min x y
        Maxima -> max x y
    )
  times Infinity _ = Infinity
  times _ Infinity = Infinity
  times (Tropical x) (Tropical y) = Tropical (Monoid.mappend x y)
  fromNatural 0 = zero
  fromNatural _ = one

instance forall e a. (Ord a, Monoid.Monoid a, Extremum e) => Star (Tropical e a) where
  star _ = one
