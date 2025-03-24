-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--   fmap = map

-- instance Functor Maybe where
--   fmap f (Just x) = Just (f x)
--   fmap f Nothing = Nothing

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) =
    Node (f x) (fmap f left) (fmap f right)
