module Data.Field
  ( -- * Field typeclass
    Field(..)
  , fromRational
  ) where

import Prelude hiding (fromRational, quot)
import Data.Complex (Complex)
import Data.Euclidean (Euclidean, WrappedFractional, quot)
import Data.Semiring (Ring, one)
import Foreign.C.Types (CDouble, CFloat)
import GHC.Real (Ratio((:%)))

---------------------------------------------------------------------
-- Classes
---------------------------------------------------------------------

-- | A field is a ring with a multiplicative inverse for any non-zero element.
class (Euclidean a, Ring a) => Field a where

  -- | Divide two elements of a 'Field'.
  -- For any 'Prelude.Fractional' type, this is the same as '(Prelude./)'.
  --
  --     @x `divide` y = x `times` 'recip' y@
  divide :: a -> a -> a
  divide = quot
  {-# INLINE divide #-}

  -- | Invert an element of a 'Field'.
  -- For any 'Prelude.Fractional' type, this is the same as 'Prelude.recip'.
  --
  --     @'recip' x `times` x = 'one'@
  recip :: a -> a
  recip = quot one
  {-# INLINE recip #-}

  -- | Infix shorthand for 'divide'.
  (/) :: a -> a -> a
  (/) = quot
  {-# INLINE (/) #-}

infixl 7 `divide`
infixl 7 /

-- | Convert from rational to field.
fromRational :: (Integral a, Field a, Num a) => Rational -> a
fromRational (x :% y) = quot (fromIntegral x) (fromIntegral y)
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

instance (Eq a, Fractional a) => Field (WrappedFractional a)

instance (Eq a, Fractional a, Ring a) => Field (Complex a)
