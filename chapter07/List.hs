-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- data List a = Empty | Cons {listhead :: a, listTail :: List a}
--   deriving (Show, Read, Eq, Ord)

infixr 5 :-:

data List a = Empty | a :-: (List a) deriving (Show)

infixr 5 ^++

(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
