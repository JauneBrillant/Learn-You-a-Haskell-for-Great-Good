data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x == y = True
  | x < y = treeElem x left
  | x > y = treeElem x right

Node
  5
  ( Node
      3
      (Node 1 EmptyTree EmptyTree)
      (Node 4 EmptyTree EmptyTree)
  )
  ( Node
      7
      (Node 6 EmptyTree EmptyTree)
      (Node 8 EmptyTree EmptyTree)
  )
