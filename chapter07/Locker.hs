-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

import Data.Map qualified as Map

data LockerState = Taken | Free deriving (Eq, Show)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "JAH3I")),
      (103, (Free, "IQSA9")),
      (105, (Free, "QOTSA")),
      (109, (Taken, "893JJ")),
      (110, (Taken, "99292"))
    ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "this number: " ++ show lockerNumber ++ " does not exist!"
  (Just (state, code)) ->
    if state == Taken
      then Left $ "this number: " ++ show lockerNumber ++ " is already used!"
      else Right code
