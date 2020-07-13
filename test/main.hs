{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE DerivingVia #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad ((>=>),forM)
import Control.Applicative (liftA2)
import Data.Complex
import Data.Either
import Data.Euclidean hiding (gcd)
import Data.Fixed
import Data.Functor.Const
import Data.Functor.Identity
import Data.HashMap.Strict
import Data.HashSet
import Data.Hashable
import Data.Int
import Data.IntMap.Strict
import Data.IntSet
import Data.Map
import Data.Monoid
import Data.Ord (Down(..))
import Data.Orphans ()
import Data.Proxy (Proxy(..))
import Data.Ratio
import Data.Semigroup
import Data.Semiring
import Data.Semiring.Generic
import Data.Sequence
import Data.Set
import Data.Word
import GHC.Generics
import GHC.Natural
import qualified Data.Foldable as F
import Prelude hiding (Num(..),(^))
import System.IO.Unsafe
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Gen (suchThat)
import Test.QuickCheck.Instances ()
import Test.Tasty (defaultMain, testGroup, TestTree)
import Text.Show.Functions ()
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified GHC.Num as Num
import qualified Test.QuickCheck.Classes as QCC
import qualified Control.Exception as E

main :: IO ()
main = do
  QCC.lawsCheckMany namedTests
  F.fold pow_prop
  F.fold gcd_prop

forceError :: a -> IO (Either E.ErrorCall a)
forceError = E.try . E.evaluate

pow_prop :: [IO ()]
pow_prop = [quickCheck pow_prop1, quickCheck pow_prop2, quickCheck pow_prop3]

pow_prop1 :: Int -> Negative Int -> Property
pow_prop1 x y = ioProperty $ do
  p <- forceError (x ^ getNegative y)
  pure $ isLeft p

pow_prop2 :: Int -> Positive Int -> Property
pow_prop2 x y = ioProperty $ do
  p <- forceError (x ^ getPositive y)
  pure $ isRight p

pow_prop3 :: Int -> Property
pow_prop3 x = ioProperty $ do
  p <- forceError (x ^ 0)
  case p of
    Left e -> pure False
    Right x -> pure $ x == 1

gcd_prop :: [IO ()]
gcd_prop = [quickCheck gcd_prop1, quickCheck gcd_prop2]

gcd_prop1 :: Integer -> Integer -> Property
gcd_prop1 x y = Num.abs (fst (x `gcdExt` y)) === x `gcd` y

gcd_prop2 :: Integer -> Integer -> Property
gcd_prop2 x y = y /= 0 ==> (x * s) `mod` y === g `mod` y
  where
    (g, s) = x `gcdExt` y

type Laws = QCC.Laws

semiringLaws :: (Arbitrary a, Show a, Eq a, Semiring a) => Proxy a -> [Laws]
semiringLaws p = [QCC.semiringLaws p]

ringLaws :: (Arbitrary a, Show a, Eq a, Ring a) => Proxy a -> [Laws]
ringLaws p = [QCC.semiringLaws p, QCC.ringLaws p]

euclideanLaws :: (Arbitrary a, Show a, Eq a, Euclidean a) => Proxy a -> [Laws]
#if MIN_VERSION_quickcheck_classes(0,6,3)
euclideanLaws p = [QCC.gcdDomainLaws p, QCC.euclideanLaws p]
#else
euclideanLaws _ = []
#endif

namedTests :: [(String, [Laws])]
namedTests =
  [ ("Bool", semiringLaws pBool)
--  , ("Double", ringLaws pDouble) -- needs to be within some epsilon
--  , ("Float", ringLaws pFloat)   -- needs to be within some epsilon
--  , ("Complex", ringLaws pComplex) -- needs to be within some epsilon
  , ("Mod2", semiringLaws pMod2 ++ ringLaws pMod2 ++ euclideanLaws pMod2)
  , ("Int", semiringLaws pInt ++ ringLaws pInt ++ euclideanLaws pInt)
  , ("Word", semiringLaws pWord ++ ringLaws pWord ++ euclideanLaws pWord)
  , ("Natural", semiringLaws pNatural ++ euclideanLaws pNatural)
  , ("Rational", semiringLaws pRational ++ ringLaws pRational ++ euclideanLaws pRational)
  , ("()", semiringLaws pInt8 ++ ringLaws pUnit)
  , ("Maybe", semiringLaws pMaybe)
  , ("PosRatio", semiringLaws pPosRatio)
  , ("IO", semiringLaws pIO ++ ringLaws pIO)
  , ("Fixed", ringLaws pFixed)
  , ("Identity", ringLaws pIdentity)
  , ("Dual", ringLaws pDual)
  , ("(->)", ringLaws pFunction)
  , ("Down", ringLaws pDown)
  , ("Const", ringLaws pConst)
--  , ("IntMap", semiringLaws pIntMap) -- needs newtypes
  , ("Set", semiringLaws pSet)
--  , ("IntSet", semiringLaws pIntSet) -- needs newtypes
  , ("HashSet", semiringLaws pHashSet)
  , ("HashMap", semiringLaws pHashMap)
  , ("Map", semiringLaws pMap)
  , ("Predicate", semiringLaws pPredicate)
  , ("Equivalence", semiringLaws pEquivalence)
  , ("Op", semiringLaws pOp ++ ringLaws pOp)
  , ("Ap", semiringLaws pAp ++ ringLaws pAp)

  , ("IntSet Sum", semiringLaws pIntSetSum)
  , ("IntSet Product", semiringLaws pIntSetProduct)
  , ("IntSet Min", semiringLaws pIntSetMin)
  , ("IntSet Max", semiringLaws pIntSetMax)

  , ("IntMap Sum", semiringLaws pIntMapSum)
  , ("IntMap Product", semiringLaws pIntMapProduct)
  , ("IntMap Min", semiringLaws pIntMapMin)
  , ("IntMap Max", semiringLaws pIntMapMax)

  , ("AB", semiringLaws pAB)
  ]

#if !(MIN_VERSION_base(4,12,0))
newtype Ap f a = Ap { getAp :: f a }
  deriving (Eq, Functor, Applicative, Show)
instance (Semiring a, Applicative f) => Semiring (Ap f a) where
  zero = pure zero
  one = pure one
  plus = liftA2 plus
  times = liftA2 times
#endif
deriving instance (Arbitrary (f a)) => Arbitrary (Ap f a)

newtype Predicate a = Predicate (a -> Bool)
  deriving (Eq, Show)
deriving instance Semiring (Predicate a)
deriving instance (Arbitrary a, CoArbitrary a) => Arbitrary (Predicate a)

newtype Equivalence a = Equivalence (a -> a -> Bool)
  deriving (Eq, Show)
deriving instance Semiring a => Semiring (Equivalence a)
deriving instance (Arbitrary a, CoArbitrary a) => Arbitrary (Equivalence a)

newtype Op a b = Op (b -> a)
  deriving (Eq, Show)
deriving instance Semiring a => Semiring (Op a b)
deriving instance Ring a => Ring (Op a b)
deriving instance (Arbitrary a, CoArbitrary a, CoArbitrary b) => Arbitrary (Op a b)

pAp = Proxy :: Proxy (Ap Identity Int)
pPredicate = Proxy :: Proxy (Predicate Int)
pEquivalence = Proxy :: Proxy (Equivalence Int)
pOp = Proxy :: Proxy (Op Int Int)

deriving instance Arbitrary a => Arbitrary (Down a)

-- | Rationals constrained to being positive
newtype PosRatio a = PosRatio { getPosRatio :: Ratio a }
  deriving (Eq, Show, Semiring)

instance (Arbitrary a, Integral a, Ord a) => Arbitrary (PosRatio a) where
  arbitrary =
    let nm = suchThat (arbitrary :: Gen a) (> 0)
        dm = suchThat (arbitrary :: Gen a) (> 0)
    in PosRatio <$> liftA2 (%) nm dm

instance (Arbitrary a, Eq b) => Eq (a -> b) where
  (==) f g =
    let x = generate arbitrary :: IO a
        x' = unsafePerformIO x
    in f x' == g x'

instance Show (Endo a) where
  show _ = "<<<Endo>>>"

deriving instance (Arbitrary a, Eq a) => Eq (Endo a)

-- no
instance Show (IO a) where
  show _ = "<<<IO action>>>"

-- no no
instance Eq a => Eq (IO a) where
  x == y = unsafePerformIO $ liftA2 (==) x y

-- this is fine
instance Arbitrary a => Arbitrary (IO a) where
  arbitrary = fmap pure (arbitrary :: Gen a)

deriving instance Hashable a => Hashable (Sum a)

deriving instance Arbitrary (IntSetOf a)
deriving instance Arbitrary v => Arbitrary (IntMapOf k v)

deriving instance Arbitrary Mod2

pBool = Proxy :: Proxy Bool
pDouble = Proxy :: Proxy Double
pFloat = Proxy :: Proxy Float
pInt = Proxy :: Proxy Int
pInt8 = Proxy :: Proxy Int8
pInt16 = Proxy :: Proxy Int16
pInt32 = Proxy :: Proxy Int32
pInt64 = Proxy :: Proxy Int64
pInteger = Proxy :: Proxy Integer
pNatural = Proxy :: Proxy Natural
pWord = Proxy :: Proxy Word
pWord8 = Proxy :: Proxy Word8
pWord16 = Proxy :: Proxy Word16
pWord32 = Proxy :: Proxy Word32
pWord64 = Proxy :: Proxy Word64
pUnit = Proxy :: Proxy ()
pMaybe = Proxy :: Proxy (Maybe Int)
pPosRatio = Proxy :: Proxy (PosRatio Int)
pIO = Proxy :: Proxy (IO Int)
pComplex = Proxy :: Proxy (Complex Double)
pFixed = Proxy :: Proxy (Fixed E0)
pMin = Proxy :: Proxy (Min Int)
pMax = Proxy :: Proxy (Max Int)
pIdentity = Proxy :: Proxy (Identity Int)
pDual = Proxy :: Proxy (Dual Int)
pEndo = Proxy :: Proxy (Endo (Sum Int))
pSum  = Proxy :: Proxy (Sum Int)
pProduct = Proxy :: Proxy (Product Int)
pDown = Proxy :: Proxy (Down Int)
pSeq = Proxy :: Proxy (Seq Int)
pSet = Proxy :: Proxy (Set (Sum Int))
pHashSet = Proxy :: Proxy (HashSet (Sum Int))
pFunction = Proxy :: Proxy (Int -> Int)
pMap = Proxy :: Proxy (Map (Sum Int) Int)
pHashMap = Proxy :: Proxy (HashMap (Sum Int) Int)
pConst = Proxy :: Proxy (Const Int Int)
pAlt = Proxy :: Proxy (Alt Maybe Int)
pRational = Proxy :: Proxy Rational
pMod2 = Proxy :: Proxy Mod2
pAB = Proxy :: Proxy AB

pIntSetSum = Proxy :: Proxy (IntSetOf (Sum Int))
pIntSetProduct = Proxy :: Proxy (IntSetOf (Product Int))
pIntSetMin = Proxy :: Proxy (IntSetOf (Min Int))
pIntSetMax = Proxy :: Proxy (IntSetOf (Max Int))

pIntMapSum = Proxy :: Proxy (IntMapOf (Sum Int) Int)
pIntMapProduct = Proxy :: Proxy (IntMapOf (Product Int) Int)
pIntMapMin = Proxy :: Proxy (IntMapOf (Min Int) Int)
pIntMapMax = Proxy :: Proxy (IntMapOf (Max Int) Int)

newtype A = A Integer
  deriving (Show, Eq, Semiring, Arbitrary)
newtype B = B Integer
  deriving (Show, Eq, Semiring, Arbitrary)

data AB = AB A B
  deriving (Show, Eq, Generic)
#if MIN_VERSION_base(4,12,0)
  deriving (Semiring) via (GenericSemiring AB)
#else
instance Semiring AB where
  zero = gzero
  one = gone
  plus = gplus
  times = gtimes
  fromNatural = gfromNatural
#endif

instance Arbitrary AB where
  arbitrary = AB <$> arbitrary <*> arbitrary
  shrink = genericShrink
