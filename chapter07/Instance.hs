class Eqq a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x === y = not (x /== y)
  x /== y = not (x === y)

data TrafficLight = Red | Yellow | Green

instance Eqq TrafficLight where
  Red === Red = True
  Green === Green = True
  Yellow === Yellow = True
  _ === _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- data Maybe a = Nothing | Just a
instance (Eqq m) => Eqq (Maybe m) where
  Just x === Just y = True
  Nothing === Nothing = True
  _ === _ = False
