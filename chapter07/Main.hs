import Shapes

-- main :: IO ()
-- main = do
-- let shape = nudge (baseCircle 30) 10 20
-- print shape

data IntMaybe = INothing | IJust Int

-- data Car = Car String String Int

data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }

-- data Car a b c = Car
--   { company :: a,
--     model :: b,
--     year :: c
--   }
--   deriving (Show)

tellCar :: Car -> String
tellCar car = company car ++ ", " ++ model car ++ " is made by " ++ show (year car) ++ "."
