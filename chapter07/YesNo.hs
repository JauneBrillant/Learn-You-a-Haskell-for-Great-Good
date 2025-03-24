data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

data TrafficLight = Red | Yellow | Green

class YesNo a where
  yesno :: a -> Bool
  yesnoIf :: (YesNo y) => y -> a -> a -> a
  yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
      then yesResult
      else noResult

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id -- id: 引数を一つとって同じものを返す

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno Yellow = False
  yesno Green = True
