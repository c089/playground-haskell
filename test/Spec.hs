import Test.Hspec
import BinaryTree
import Data.List (unfoldr)

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

  describe "Chapter 12 / unfolds" $ do
    describe "iterate" $ do
      let
        myIterate :: (a -> a) -> a -> [a]
        myIterate f a = a : myIterate f (f a)

      it "builds infinite list of things" $ do
        take 5 (myIterate id (1::Integer)) `shouldBe` ([1,1,1,1,1] :: [Integer])
        take 5 (myIterate id (5::Integer)) `shouldBe` ([5,5,5,5,5] :: [Integer])
        take 5 (myIterate (+ 1) (0::Integer)) `shouldBe` ([0,1,2,3,4] :: [Integer])

    describe "unfoldr" $ do
      let
        myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
        myUnfoldr f b = case f b of
          Just (a,b2) -> a : myUnfoldr f b2
          Nothing -> []
        add1 = (\b -> Just (b, b+1))
        result = myUnfoldr add1 (1::Integer)

      it "can be used like iterate, but more general" $ do
       take 5 result `shouldBe` ([1,2,3,4,5] :: [Integer])

      it "works exactly like Data.List.unfoldr" $ do
        take 1000 result `shouldBe` (take 1000 $ unfoldr add1 (1::Integer))

      it "ends the list when function returns Nothing" $ do
        let
          add1Until10 = (\b -> if b <= 10 then Just (b, b+1) else Nothing)
        myUnfoldr add1Until10 (1::Integer) `shouldBe` [1,2,3,4,5,6,7,8,9,10]

    describe  "myBetterIterate" $ do
      it "is defined in terms of myUnfoldr" $ do
        let
          myBetterIterate :: (a -> a) -> a -> [a]
          myBetterIterate f = unfoldr (\a -> Just (a, f a))
        take 5 (myBetterIterate (+ 1) (0::Integer)) `shouldBe` ([0,1,2,3,4] :: [Integer])
