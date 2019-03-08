module FunctorSpec(spec, Identity) where

import Test.Hspec
import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f
functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary ( Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return (Pair a1 a2)

spec :: Spec
spec = do
  describe "Chapter 16 (Functor)"  $ do
    describe "Identity" $ do
      let
        idProperty :: (Identity String) -> Bool
        idProperty  = functorIdentity
        composeProperty x = functorCompose (+1) (*2) ( x:: (Identity Int) )

      it "abides the identity law" $ property idProperty

      it "abides the composition law" $ property composeProperty


    describe "Pair" $ do
      let
        idProperty :: (Pair Int) -> Bool
        idProperty = functorIdentity
        composeProperty x = functorCompose (+1) (*2) ( x :: (Pair Int) )

      it "abides the identity law" $ property idProperty

      it "abides the composition law" $ property composeProperty
