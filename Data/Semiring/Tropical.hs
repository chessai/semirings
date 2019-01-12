{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-----------------------------------------------------------------------------
-- |
--   A tropical semiring is an extension of another totally ordered
--   semiring with the operations of minimum or maximum as addition.
--   The extended semiring is given positive or negative infinity as
--   its 'zero' element, so that the following holds:
--
--   @
--     'plus' 'Infinity' y = y
--     'plus' x 'Infinity' = x
--   @
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
  ) where

import Control.Applicative
  ( Alternative(empty,(<|>))
#if !MIN_VERSION_base(4,8,0)
  , Applicative(..)
#endif

#if MIN_VERSION_base(4,8,0)
  , liftA2
#endif
  )
import Control.Monad (MonadPlus(mzero,mplus))
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail(fail))
#endif
import Control.Monad.Fix (MonadFix(mfix))
#if MIN_VERSION_base(4,8,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
#if MIN_VERSION_base(4,7,0)
import Data.Data (Data)
#endif
#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif
#if !MIN_VERSION_base(4,10,0)
import Data.Monoid (Monoid(mempty,mappend))
#endif
#if MIN_VERSION_base(4,6,0)
import Data.Proxy (Proxy(Proxy))
#endif
import Data.Semiring (Semiring(..))
#if MIN_VERSION_base(4,7,0)
import Data.Typeable (Typeable)
#endif

-- just for hadocks
#if MIN_VERSION_base(4,12,0)
import qualified Data.Monoid as Monoid
import qualified GHC.Natural as Natural
#endif

#if !MIN_VERSION_base(4,6,0)
-- | On older GHCs, 'Data.Proxy.Proxy' is not polykinded, so we provide our own proxy type for 'Extrema'.
data EProxy (e :: Extrema) = EProxy
#endif

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
#if !MIN_VERSION_base(4,6,0)
  extremum :: EProxy e -> Extrema
#else
  extremum :: Proxy e -> Extrema
#endif

instance Extremum 'Minima where
  extremum _ = Minima
  {-# INLINE extremum #-} -- just to be safe

instance Extremum 'Maxima where
  extremum _ = Maxima
  {-# INLINE extremum #-} -- just to be safe

-- | The tropical semiring. @'Tropical' ''Minima' a@ is equivalent to the semiring
--     \[
--     (a \cup \{+\infty\}, \oplus, \otimes)
--     \]
--     where
--     \[
--     x \oplus y = min\{x,y\},
--     \]
--     \[
--     x \otimes y = x + y.
--     \]
--
--   @'Tropical' ''Maxima' a@ is equivalent to the semiring
--     \[
--     (a \cup \{-\infty\}, \oplus, \otimes)
--     \]
--     where
--     \[
--     x \oplus y = max\{x,y\},
--     \]
--     \[
--     x \otimes y = x + y.
--     \]
--
-- In literature, the 'Semiring' instance of the 'Tropical' semiring lifts
-- the underlying semiring's additive structure. One might ask why this lifting doesn't
-- instead witness isn't 'Monoid', since we only lift 'zero' and 'plus' - the reason is
-- that usually the additive structure of a semiring is monotonic, i.e.
-- @a '+' ('min' b c) == 'min' (a '+' b) (a '+' c)@, but in general this is not true.
-- For example, lifting 'Monoid.Product' 'Natural.Natural' or 'Monoid.Product' 'Word' into 'Tropical' is lawful,
-- but 'Monoid.Product' 'Int' is not, lacking distributivity: @(-1) '*' ('min' 0 1) '/=' 'min' ((-1) '*' 0) ((-1) '*' 1)@.
-- So, we deviate from literature and instead
-- witness the lifting of a 'Monoid', so the user must take care to ensure
-- that their implementation of 'mappend' is monotonic.
data Tropical :: Extrema -> * -> * where
  Infinity :: Tropical e a
  Tropical :: a -> Tropical e a
#if MIN_VERSION_base(4,7,0)  
  deriving (Typeable)
#endif

#if MIN_VERSION_base(4,7,0)
deriving instance (Typeable e, Data a) => Data (Tropical e a)
#endif
deriving instance Eq a => Eq (Tropical e a)
deriving instance Foldable (Tropical e)
deriving instance Functor (Tropical e)
deriving instance Read a => Read (Tropical e a)
deriving instance Show a => Show (Tropical e a)
deriving instance Traversable (Tropical e)

instance forall e a. (Ord a, Extremum e) => Ord (Tropical e a) where
  compare Infinity Infinity         = EQ
#if !MIN_VERSION_base(4,6,0)  
  compare Infinity _                = case extremum (EProxy :: EProxy e) of
    Minima -> LT
    Maxima -> GT
  compare _ Infinity                = case extremum (EProxy :: EProxy e) of
    Minima -> GT
    Maxima -> LT
#else
  compare Infinity _                = case extremum (Proxy :: Proxy e) of
    Minima -> LT
    Maxima -> GT
  compare _ Infinity                = case extremum (Proxy :: Proxy e) of
    Minima -> GT
    Maxima -> LT
#endif  
  compare (Tropical x) (Tropical y) = compare x y

instance Applicative (Tropical e) where
  pure = Tropical

  Tropical f <*> t = fmap f t
  Infinity <*>   _ = Infinity

#if MIN_VERSION_base(4,10,0)
  liftA2 f (Tropical x) (Tropical y) = Tropical (f x y)
  liftA2 _ _ _ = Infinity
#endif

  Tropical _ *> t = t
  Infinity *> _   = Infinity

instance Monad (Tropical e) where
  return = pure 
  
  Tropical x >>= k = k x
  Infinity >>= _   = Infinity

  (>>) = (*>)

  fail _ = Infinity

instance MonadFix (Tropical e) where
  mfix f = let a = f (unTropical a) in a
    where
      unTropical (Tropical x) = x
      unTropical Infinity     = errorWithoutStackTrace' "mfix Tropical: Infinity"

errorWithoutStackTrace' :: [Char] -> a
errorWithoutStackTrace' =
#if MIN_VERSION_base(4,9,0)
  errorWithoutStackTrace
#else
  error
#endif

#if MIN_VERSION_base(4,9,0)
instance MonadFail (Tropical e) where
  fail _ = Infinity
#endif

#if MIN_VERSION_base(4,8,0)
instance MonadZip (Tropical e) where
  mzipWith = liftA2
#endif

instance Alternative (Tropical e) where
  empty = Infinity
  Infinity <|> r = r
  l        <|> _ = l

instance MonadPlus (Tropical e) where
  mzero = empty
  mplus = (<|>)

instance forall e a. (Ord a, Monoid a, Extremum e) => Semiring (Tropical e a) where
  zero = Infinity
  one  = Tropical mempty
  plus Infinity y = y
  plus x Infinity = x
#if !MIN_VERSION_base(4,6,0)
  plus (Tropical x) (Tropical y) = Tropical
    ( case extremum (EProxy :: EProxy e) of
        Minima -> min x y
        Maxima -> max x y
    )
#else
  plus (Tropical x) (Tropical y) = Tropical
    ( case extremum (Proxy :: Proxy e) of
        Minima -> min x y
        Maxima -> max x y
    )
#endif
  times Infinity _ = Infinity
  times _ Infinity = Infinity
  times (Tropical x) (Tropical y) = Tropical (mappend x y)

