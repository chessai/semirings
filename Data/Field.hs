module Data.Field
  ( -- * Field typeclass
    Field
  , divide
  , fromRational
  , recip
  , (/)
  ) where

import Prelude hiding (fromRational, negate, quot, recip, (/))
import Data.Semiring (fromNatural, negate, one)
import GHC.Real (Ratio((:%)))
import Data.Euclidean (Field, quot)

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
fromRational (x :% y)
  | x >= 0 && y >= 0 = quot (fromN x) (fromN y)
  | x <  0 && y >= 0 = quot (negate (fromN (negate x))) (fromN y)
  | x >= 0 && y <  0 = quot (negate (fromN x)) (fromN (negate y))
  | otherwise        = quot (fromN (negate x)) (fromN (negate y))
  where
    fromN = fromNatural . fromInteger
{-# INLINE fromRational #-}
