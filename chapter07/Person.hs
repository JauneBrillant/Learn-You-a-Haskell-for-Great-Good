-- data Person = Person String String Int Float String String deriving (Show)

-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname

-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname

-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age

-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ phonenum _) = phonenum

-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ x) = x

-- data Person = Person
--   { firstName :: String,
--     nastName :: String,
--     age :: Int,
--     height :: Float,
--     phoneNumber :: String,
--     flavor :: String
--   }
--   deriving (Show)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Eq, Show, Read)

mikeD = Person {firstName = "Micheal", lastName = "Diamond", age = 43}

adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}

mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

mysteryDude =
  "Person { firstName =\"Michael\""
    ++ ", lastName =\"Diamond\""
    ++ ", age = 43}"

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

phoneBook :: [(String, String)]
phoneBook =
  [ ("satou", "111-1111"),
    ("takeshi", "222-2222"),
    ("shinji", "333-3333")
  ]
