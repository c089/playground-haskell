module MyOptionalSpec(spec) where

import Test.Hspec
import Data.Monoid
import MyOptional (Optional(Only))

spec :: Spec
spec = do
  describe "Chapter12 (MyOptional)"  $ do
      it "works" $ do
        (Only (Sum ( 2::Integer ))) `shouldBe` (Only (Sum (1::Integer)) `mappend` Only (Sum (1::Integer)))
