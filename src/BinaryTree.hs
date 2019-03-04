module BinaryTree (BinaryTree(Leaf,Node), mapTree, preorder, postorder, inorder, foldTree) where

data BinaryTree a = Leaf
                    | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf =
  Leaf
mapTree f (Node Leaf a Leaf) =
  Node Leaf (f a) Leaf
mapTree f (Node left a right ) =
  Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left ++ preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b tree = foldr f b $ inorder tree
