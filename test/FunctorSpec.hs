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

spec :: Spec
spec = do
  describe "Chapter (Functor)"  $ do
    describe "Identity functor" $ do
      let
        idProperty :: (Identity String) -> Bool
        idProperty  = functorIdentity
        composeProperty x = functorCompose (+5) (*2) ( x:: (Identity Int) )

      it "abides the identity law" $ property idProperty

      it "abides the composition law" $ property composeProperty
