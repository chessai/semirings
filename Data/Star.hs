module Data.Star
  ( Star(..)
  ) where

import Data.Bool (Bool(..))
import Data.Complex (Complex(..))
import Data.Function (id, (.))
import Data.Functor (Functor(..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word, Word8, Word16, Word32, Word64)

import Data.Semiring

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

