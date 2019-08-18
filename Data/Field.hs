{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Field
  ( -- * Field typeclass
    Field(..)
  , fromRational
  , (/)
  ) where

import Prelude hiding (fromRational, recip, (/))
import Data.Complex (Complex((:+)))
import Data.Euclidean (WrappedFractional)
import Data.Semiring (Ring, one, plus, times)
import Foreign.C.Types (CDouble, CFloat)
import GHC.Real (Ratio((:%)))
import qualified GHC.Real as R (recip, (/))

---------------------------------------------------------------------
-- Classes
---------------------------------------------------------------------

-- | A field is a ring with a multiplicative inverse for any non-zero element.
class Ring a => Field a where
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL (divide | recip) #-}
#endif

  -- | Divide two elements of a 'Field'.
  -- For any 'Prelude.Fractional' type, this is the same as '(Prelude./)'.
  --
  --     @x `divide` y = x `times` 'recip' y@
  divide :: a -> a -> a
  divide x y = times x (recip y)
  {-# INLINE divide #-}

  -- | Invert an element of a 'Field'.
  -- For any 'Prelude.Fractional' type, this is the same as 'Prelude.recip'.
  --
  --     @'recip' x `times` x = 'one'@
  recip :: a -> a
  recip x = divide one x
  {-# INLINE recip #-}

-- | Convert from rational to field.
fromRational :: (Integral a, Field a, Num a) => Rational -> a
fromRational (x :% y) = divide (fromIntegral x) (fromIntegral y)
{-# INLINE fromRational #-}

-- | Infix shorthand for 'divide'.
infixl 7 `divide`, /
(/) :: Field a => a -> a -> a
(/) = divide
{-# INLINE (/) #-}

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance Field () where
  divide _ _ = ()
  {-# INLINE divide #-}
  recip _ = ()
  {-# INLINE recip #-}

instance Field Float where
  divide = (R./)
  {-# INLINE divide #-}
  recip = R.recip
  {-# INLINE recip #-}

instance Field Double where
  divide = (R./)
  {-# INLINE divide #-}
  recip = R.recip
  {-# INLINE recip #-}

instance Field CFloat where
  divide = (R./)
  {-# INLINE divide #-}
  recip = R.recip
  {-# INLINE recip #-}

instance Field CDouble where
  divide = (R./)
  {-# INLINE divide #-}
  recip = R.recip
  {-# INLINE recip #-}

instance Integral a => Field (Ratio a) where
  divide = (R./)
  {-# INLINE divide #-}
  recip = R.recip
  {-# INLINE recip #-}

instance (Eq a, Fractional a) => Field (WrappedFractional a) where
  divide = (R./)
  {-# INLINE divide #-}
  recip = R.recip
  {-# INLINE recip #-}

instance (Eq a, Fractional a, Ring a) => Field (Complex a) where
  recip (0 :+ _) = error "divide: zero denominator"
  recip (x :+ y) = (x R./ xxyy) :+ (-y R./ xxyy)
    where
      xxyy = plus (times x x) (times y y)
  {-# INLINE recip #-}
