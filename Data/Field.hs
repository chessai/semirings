module Data.Field
  ( -- * Field typeclass
    Field(..)
  , fromRational
  ) where

import Prelude hiding (fromRational, (/))
import Data.Complex (Complex)
import qualified Data.Euclidean as R (Euclidean, WrappedFractional, quot)
import qualified Data.Semiring as R (Ring, fromNatural, negate, one)
import Foreign.C.Types (CDouble, CFloat)
import GHC.Real (Ratio((:%)))

---------------------------------------------------------------------
-- Classes
---------------------------------------------------------------------

-- | A field is a ring with a multiplicative inverse for any non-zero element.
class (R.Euclidean a, R.Ring a) => Field a where

  -- | Divide two elements of a 'Field'.
  -- For any 'Prelude.Fractional' type, this is the same as '(Prelude./)'.
  --
  --     @x `divide` y = x `times` 'recip' y@
  divide :: a -> a -> a
  divide = R.quot
  {-# INLINE divide #-}

  -- | Invert an element of a 'Field'.
  -- For any 'Prelude.Fractional' type, this is the same as 'Prelude.recip'.
  --
  --     @'recip' x `times` x = 'one'@
  recip :: a -> a
  recip = R.quot R.one
  {-# INLINE recip #-}

  -- | Infix shorthand for 'divide'.
  (/) :: a -> a -> a
  (/) = R.quot
  {-# INLINE (/) #-}

infixl 7 `divide`
infixl 7 /

-- | Convert from rational to field.
fromRational :: Field a => Rational -> a
fromRational (x :% y)
  | x >= 0 && y >= 0 = R.quot (fromN x) (fromN y)
  | x <  0 && y >= 0 = R.quot (R.negate (fromN (negate x))) (fromN y)
  | x >= 0 && y <  0 = R.quot (R.negate (fromN x)) (fromN (negate y))
  | otherwise        = R.quot (fromN (negate x)) (fromN (negate y))
  where
    fromN = R.fromNatural . fromInteger
{-# INLINE fromRational #-}

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance Field ()

instance Field Float

instance Field Double

instance Field CFloat

instance Field CDouble

instance Integral a => Field (Ratio a)

instance (Eq a, Fractional a) => Field (R.WrappedFractional a)

instance (Eq a, Fractional a, R.Ring a) => Field (Complex a)
