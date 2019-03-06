module ExampleSpec(spec) where

import Test.Hspec

spec :: Spec
spec = describe "Haskell Playground" $ do
    it "allows me to quickly start test driving Haskell code" $ do
      True `shouldBe` True
