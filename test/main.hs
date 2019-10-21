{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}

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
import Data.Sequence
import Data.Set
import Data.Word
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

pAp = p @(Ap Identity Int)
pPredicate = p @(Predicate Int)
pEquivalence = p @(Equivalence Int)
pOp = p @(Op Int Int)

deriving instance Arbitrary a => Arbitrary (Down a)

-- | Rationals constrained to being positive
newtype PosRatio a = PosRatio { getPosRatio :: Ratio a }
  deriving (Eq, Show, Semiring)

instance (Arbitrary a, Integral a, Ord a) => Arbitrary (PosRatio a) where
  arbitrary =
    let nm = suchThat (arbitrary :: Gen a) (> 0)
        dm = suchThat (arbitrary :: Gen a) (> 0)
    in PosRatio <$> liftA2 (%) nm dm

p :: forall a. Proxy a
p = Proxy

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

pBool = p @Bool
pDouble = p @Double
pFloat = p @Float
pInt = p @Int
pInt8 = p @Int8
pInt16 = p @Int16
pInt32 = p @Int32
pInt64 = p @Int64
pInteger = p @Integer
pNatural = p @Natural
pWord = p @Word
pWord8 = p @Word8
pWord16 = p @Word16
pWord32 = p @Word32
pWord64 = p @Word64
pUnit = p @()
pMaybe = p @(Maybe Int)
pPosRatio = p @(PosRatio Int)
pIO = p @(IO Int)
pComplex = p @(Complex Double)
pFixed = p @(Fixed E0)
pMin = p @(Min Int)
pMax = p @(Max Int)
pIdentity = p @(Identity Int)
pDual = p @(Dual Int)
pEndo = p @(Endo (Sum Int))
pSum  = p @(Sum Int)
pProduct = p @(Product Int)
pDown = p @(Down Int)
pSeq = p @(Seq Int)
pSet = p @(Set (Sum Int))
pHashSet = p @(HashSet (Sum Int))
pFunction = p @(Int -> Int)
pMap = p @(Map (Sum Int) Int)
pHashMap = p @(HashMap (Sum Int) Int)
pConst = p @(Const Int Int)
pAlt = p @(Alt Maybe Int)
pRational = p @Rational
pMod2 = p @Mod2

pIntSetSum = p @(IntSetOf (Sum Int))
pIntSetProduct = p @(IntSetOf (Product Int))
pIntSetMin = p @(IntSetOf (Min Int))
pIntSetMax = p @(IntSetOf (Max Int))

pIntMapSum = p @(IntMapOf (Sum Int) Int)
pIntMapProduct = p @(IntMapOf (Product Int) Int)
pIntMapMin = p @(IntMapOf (Min Int) Int)
pIntMapMax = p @(IntMapOf (Max Int) Int)
