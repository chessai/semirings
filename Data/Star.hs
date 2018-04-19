module Data.Star
  ( Star(..)
  ) where

import Data.Bool (Bool(..))
import Data.Function (id, (.))
import Data.Monoid
import qualified Data.Vector as Vector

import Data.Semiring
import Data.Semiring.Poly

import Prelude hiding (Num(..))

-- | A <https://en.wikipedia.org/wiki/Semiring#Star_semirings Star semiring>
-- adds one operation, 'star' to a 'Semiring', such that it follows the
-- law:
--
-- @'star' x = 'one' '+' x '*' 'star' x = 'one' '+' 'star' x '*' x@
--
-- Another operation, 'aplus', can be defined in terms of 'star':
--
-- @'aplus' x = x '*' 'star' x@

class (Semiring a) => Star a where
  {-# MINIMAL star | aplus #-} 
  star :: a -> a
  star a = one `plus` aplus a

  aplus :: a -> a
  aplus a = a `times` star a

instance Star b => Star (a -> b) where
  star  = (.) star
  aplus = (.) aplus

instance Star Bool where
  star _  = True
  aplus   = id

instance Star () where
  star  _ = ()
  aplus _ = ()

instance (Eq a, Monoid a) => Star (Endo a) where
  star (Endo f) = Endo converge
    where
      converge inp = mappend inp (if inp == next then inp else converge next)
        where
          next = mappend inp (f inp)

-- this is horribly inefficient.
instance (Star a) => Star (Poly a) where
  star (Poly v)
    | Vector.null v = one
    | otherwise = Poly r
    where
      r = Vector.cons xst $ Vector.map (xst *) (Vector.unsafeTail v * r)
      xst = star (Vector.unsafeHead v)
