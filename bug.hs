{-# LANGUAGE FlexibleInstances #-}

instance (Show a, Show b) => Show (a, b) where
  show (a, b) = "(" ++ show a ++ "," ++ show b ++ ")"

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (c, d) = a == c && b == d

data Tree a = Node a [Tree a] | Leaf

instance (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show (Node a children) = "Node " ++ show a ++ " " ++ show children

instance (Eq a) => Eq (Tree a) where
  Leaf == Leaf = True
  (Node a children1) == (Node b children2) = a == b && children1 == children2
  _ == _ = False

main :: IO ()
main = do
  let tree1 = Node 1 [Node 2 [Leaf], Node 3 [Leaf, Leaf]]
  let tree2 = Node 1 [Node 2 [Leaf], Node 3 [Leaf, Leaf]]
  print (tree1 == tree2)
  print (show tree1)
  print (show tree2)