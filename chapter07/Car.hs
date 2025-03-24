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
