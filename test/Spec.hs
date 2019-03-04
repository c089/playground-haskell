import Test.Hspec
import BinaryTree

main :: IO ()
main = hspec $ do
  describe "Haskell Playground" $ do
    it "allows me to quickly start test driving Haskell code" $ do
      True `shouldBe` True

  describe "Chapter 11" $ do
    describe "Binary Trees" $ do
      it "mapping over a tree" $ do
        let tree     = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
            expected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
        mapTree (+ (1::Integer)) tree `shouldBe` expected

      describe "creating lists from trees" $ do
        let
          tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf) :: BinaryTree Integer

        it "preorder" $ do
          preorder tree `shouldBe` [2,1,3]

        it "inorder" $ do
          inorder tree `shouldBe` [1,2,3]

        it "postorder" $ do
          postorder tree `shouldBe` [1,3,2]

      describe "folding a tree" $ do
        let
          tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf) :: BinaryTree Integer

        it "works" $ do
          foldTree (+) 0 tree `shouldBe` 6
