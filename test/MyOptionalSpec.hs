module MyOptionalSpec(spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Monoid
import MyOptional (Optional(Only, Nada))

-- Wrap in newtype to prevent orphan instance warning
newtype Optional' a = Optional' (Optional a) deriving (Show, Eq)
instance Monoid a => Monoid (Optional' a) where
  mappend (Optional' a) (Optional' b) = Optional' (mappend a b)
  mempty = Optional' (mempty)
instance Semigroup a => Semigroup (Optional' a) where
  (Optional' a) <> (Optional' b) = Optional' (a <> b)
-- End of newtype wrapper boilerplate

instance (Arbitrary a) => Arbitrary (Optional' a)  where
  arbitrary = do
    a <- arbitrary
    elements [Optional' Nada, Optional' (Only a)]


type OS = Optional' String
spec :: Spec
spec = do
  describe "Chapter12 (MyOptional)"  $ do
      it "is a monoid" $ do
        let
          expected = (Only (Sum (2::Integer)))
          actual   = (Only (Sum (1::Integer)) `mappend` Only (Sum (1::Integer)))
        expected `shouldBe` actual

      it "satisfies associativity" $ property
        (monoidAssoc::OS->OS->OS->Bool)

      it "satisfies left identity " $ property
        (monoidLeftIdentity::OS->Bool)

      it "satisfies right identity" $ property
        (monoidRightIdentity::OS->Bool)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
