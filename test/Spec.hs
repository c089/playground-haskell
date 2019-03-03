import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Haskell Playground" $ do
    it "allows me to quickly start test driving Haskell code" $ do
      True `shouldBe` True
