type Name = String

type PhoneNumber = String

type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
  [ ("satou", "111-1111"),
    ("takeshi", "222-2222"),
    ("shinji", "333-3333")
  ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

type AssocList k v = [(k, v)]

findItem :: (Eq k, Eq v) => k -> v -> AssocList k v -> Maybe (k, v)
findItem key value list = if (key, value) `elem` list then Just (key, value) else Nothing

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
