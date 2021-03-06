module FunctorSpec(spec, Identity) where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Chapter 16 (Functor)"  $ do
    describe "Identity" $ do
      let
        idProperty  = functorIdentity :: (Identity String) -> Bool
        composeProperty x = functorCompose (+1) (*2) ( x:: (Identity Int) )
      it "abides the identity law" $ property idProperty
      it "abides the composition law" $ property composeProperty

    describe "Pair" $ do
      let
        idProperty = functorIdentity :: (Pair Int) -> Bool
        composeProperty x = functorCompose (+1) (*2) ( x :: (Pair Int) )
      it "abides the identity law" $ property idProperty
      it "abides the composition law" $ property composeProperty

    describe "Two" $ do
      let
        idProperty = functorIdentity :: (Two Int String) -> Bool
        composeProperty x  = functorCompose (length) (+1) (x :: (Two Int String))
      it "abides the identity law" $ property idProperty
      it "abides the compose law" $ property composeProperty

    describe "Three a b c" $ do
      let
        idProperty = functorIdentity :: (Three Bool Int String) -> Bool
        composeProperty x =  functorCompose  (length) (+1) (x :: (Three Bool Int String))

      it "abides the identity law" $ property idProperty
      it "abides the composition law" $ property composeProperty

    describe "Three' a b" $ do
      let
        idProperty = functorIdentity :: (Three' Int String) -> Bool
        composeProperty x =  functorCompose  (length) (+1) (x :: (Three' Int String))

      it "abides the identity law" $ property idProperty
      it "abides the composition law" $ property composeProperty

    describe "Four a b c d" $ do
      let
        idProperty = functorIdentity :: (Four Char Bool Int String) -> Bool
        composeProperty x =  functorCompose  (length) (+1) (x :: (Four Bool Char Int String))

      it "abides the identity law" $ property idProperty
      it "abides the composition law" $ property composeProperty

    describe "Four' a b" $ do
      let
        idProperty = functorIdentity :: (Four' Int String) -> Bool
        composeProperty x =  functorCompose  (length) (+1) (x :: (Four' Int String))

      it "abides the identity law" $ property idProperty
      it "abides the composition law" $ property composeProperty

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

data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)


data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) =  Three a b (f c)

data Three' a b = Three' a b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = (Three' a (f b1) (f b2))

data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four' a1 a2 a3 b

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)
  
