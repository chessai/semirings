{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Semiring.Matrix
  (
#if MIN_VERSION_base(4,9,0)    
    Matrix(..)
  , transpose
  , rows
  , cols
#endif
  ) where

#if MIN_VERSION_base(4,9,0)
import Control.Applicative (Applicative(..))
import Data.Coerce
import Data.Coerce.Util
import Data.Functor.Classes
import Data.Semiring
import Data.Star
import Data.Foldable hiding (sum)
import Data.Traversable
import GHC.Generics (Generic, Generic1)
import Prelude hiding ((+),(*),(-),negate, sum)

newtype Matrix f g a = Matrix { getMatrix :: f (g a) }
  deriving (Generic, Generic1, Functor, Foldable, Traversable)

instance (Applicative f, Applicative g) => Applicative (Matrix f g) where
  pure = Matrix #. pure . pure
  (<*>) =
    (coerce
      :: (f (g (a -> b))
      -> f (g a)
      -> f (g b))
      -> Matrix f g (a -> b)
      -> Matrix f g a
      -> Matrix f g b) (liftA2 (<*>))

instance (Traversable f, Applicative f, Semiring a, f ~ g) => Semiring (Matrix f g a) where
  times =
    (coerce
      :: Binary (f (g a))
      -> Binary (Matrix f g a)) mulMatrix
  plus  = liftA2 plus
  zero  = pure zero
  one   =
    (coerce
      :: (f (g a) -> f (g a))
      -> Matrix f g a
      -> Matrix f g a) (imap (\i -> imap (\j z -> if i == j then one else z))) zero
    where
      imap f = snd . mapAccumL (\ !i x -> (i + 1, f i x)) (0 :: Int)

instance (Traversable f, Applicative f, Ring a, f ~ g) => Ring (Matrix f g a) where
  negate = fmap negate

mulMatrix
  :: (Applicative n, Traversable m, Applicative m, Applicative p, Semiring a)
  => n (m a) -> m (p a) -> n (p a)
mulMatrix xs ys = fmap (\row -> fmap (sum . liftA2 times row) cs) xs
  where
    cs = sequenceA ys

transpose
  :: (Applicative g, Traversable f)
  => Matrix f g a
  -> Matrix g f a
transpose (Matrix xs) = Matrix (sequenceA xs)

-- | Convert a matrix to a nested list, in row-majour form.
rows :: (Foldable f, Foldable g) => Matrix f g a -> [[a]]
rows = foldr ((:) . toList) [] . getMatrix

-- | Convert a matrix to a nested list, in column-majour form.
cols :: (Foldable f, Foldable g) => Matrix f g a -> [[a]]
cols = foldr (foldr f (const [])) (repeat []) . getMatrix
  where
    f _ _ [] = []
    f e a (x:xs) = (e:x) : a xs

instance (Show1 f, Show1 g) => Show1 (Matrix f g) where
  liftShowsPrec (sp :: Int -> a -> ShowS) sl =
    showsNewtype "Matrix" "getMatrix" liftedTwiceSP liftedTwiceSL
    where
      liftedOnceSP :: Int -> g a -> ShowS
      liftedOnceSP = liftShowsPrec sp sl
      liftedOnceSL :: [g a] -> ShowS
      liftedOnceSL = liftShowList sp sl
      liftedTwiceSP :: Int -> f (g a) -> ShowS
      liftedTwiceSP = liftShowsPrec liftedOnceSP liftedOnceSL
      liftedTwiceSL :: [f (g a)] -> ShowS
      liftedTwiceSL = liftShowList liftedOnceSP liftedOnceSL

instance (Read1 f, Read1 g) => Read1 (Matrix f g) where
  liftReadsPrec (rp :: Int -> ReadS a) rl =
    readsNewtype "Matrix" "getMatrix" liftedTwiceRP liftedTwiceRL
    where
      liftedOnceRP :: Int -> ReadS (g a)
      liftedOnceRP = liftReadsPrec rp rl
      liftedOnceRL :: ReadS [g a]
      liftedOnceRL = liftReadList rp rl
      liftedTwiceRP :: Int -> ReadS (f (g a))
      liftedTwiceRP = liftReadsPrec liftedOnceRP liftedOnceRL
      liftedTwiceRL :: ReadS [f (g a)]
      liftedTwiceRL = liftReadList liftedOnceRP liftedOnceRL

instance (Eq1 f, Eq1 g) => Eq1 (Matrix f g) where
  liftEq (eq :: a -> b -> Bool) =
    coerce (liftEq (liftEq eq) :: f (g a) -> f (g b) -> Bool)

instance (Ord1 f, Ord1 g) => Ord1 (Matrix f g) where
  liftCompare (cmp :: a -> b -> Ordering) =
    coerce (liftCompare (liftCompare cmp) :: f (g a) -> f (g b) -> Ordering)

instance (Show1 f, Show1 g, Show a) => Show (Matrix f g a) where
  showsPrec = showsPrec1

instance (Read1 f, Read1 g, Read a) => Read (Matrix f g a) where
  readsPrec = readsPrec1

instance (Eq1 f, Eq1 g, Eq a) => Eq (Matrix f g a) where
  (==) = eq1

instance (Ord1 f, Ord1 g, Ord a) => Ord (Matrix f g a) where
  compare = compare1
#endif
