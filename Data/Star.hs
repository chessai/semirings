module Data.Star
  ( Star(..)
  ) where

import Control.Monad (ap)
import Control.Monad.Fix (fix)
import Data.Bool (Bool(..))
import Data.Function (id, (.))
import Data.Monoid

import Data.Semiring

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
      if' :: Bool -> a -> a -> a
      if' True  x _ = x
      if' False _ y = y
      converge = fix (ap mappend . ap (if' =<< ap (==) (ap mappend f)) . (. ap mappend f))
      --converge inp = mappend inp (if inp == next then inp else converge next)
      -- where
      --   next = mappend inp (f inp)
