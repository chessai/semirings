-- | A 'Field' is a 'Ring' in which all nonzero elements
--   have a multiplicative inverse.
module Data.Field
  ( -- * Field typeclass
    Field
  , divide
  , fromRational
  , recip
  , (/)
  ) where

import Prelude hiding (fromInteger, fromRational, negate, quot, recip, (/))
import Data.Euclidean (Field, quot)
import Data.Ratio (denominator, numerator)
import Data.Semiring (fromInteger, one)

---------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------

-- | Divide two elements of a 'Field'.
-- For any 'Prelude.Fractional' type, this is the same as '(Prelude./)'.
--
--     @x `divide` y = x `times` 'recip' y@
divide :: Field a => a -> a -> a
divide = quot
{-# INLINE divide #-}

infixl 7 `divide`

-- | Invert an element of a 'Field'.
-- For any 'Prelude.Fractional' type, this is the same as 'Prelude.recip'.
--
--     @'recip' x `times` x = 'one'@
recip :: Field a => a -> a
recip = quot one
{-# INLINE recip #-}

-- | Infix shorthand for 'divide'.
(/) :: Field a => a -> a -> a
(/) = quot
{-# INLINE (/) #-}

infixl 7 /

-- | Convert from rational to field.
fromRational :: Field a => Rational -> a
fromRational x = quot (fromInteger (numerator x)) (fromInteger (denominator x))
{-# INLINE fromRational #-}
