{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- A class for *-semirings (pron. "star-semirings").
--
-----------------------------------------------------------------------------
module Data.Star
  ( Star(..)
  ) where

import Data.Bool (Bool(..))
import Data.Function (id, (.), const)
import Data.Proxy (Proxy(..))
import Data.Semiring

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
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL star | aplus #-}
#endif
  star :: a -> a
  star a = one `plus` aplus a

  aplus :: a -> a
  aplus a = a `times` star a

instance Star b => Star (a -> b) where
  star  = (.) star
  aplus = (.) aplus
  {-# INLINE star #-}
  {-# INLINE aplus #-}

instance Star Bool where
  star _  = True
  aplus   = id
  {-# INLINE star #-}
  {-# INLINE aplus #-}

instance Star () where
  star  _ = ()
  aplus _ = ()
  {-# INLINE star #-}
  {-# INLINE aplus #-}

instance Star (Proxy a) where
  star _ = Proxy
  aplus _ = Proxy
  {-# INLINE star #-}
  {-# INLINE aplus #-}

instance Star Mod2 where
  star = const one

