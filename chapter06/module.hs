import Data.Char
import Data.List
import Data.Map qualified as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordsCnt = length . words $ "hey these are the words in this sentence"

sameGroup1 = group [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]

sameGroup2 = group . sort $ [4, 1, 4, 4, 3, 3, 3, 2, 2, 4]

wordNums1 :: String -> [(String, Int)]
wordNums1 = map (\ws -> (head ws, length ws)) . group . sort . words

wordNums2 :: String -> [(String, Int)]
wordNums2 xs = map (\xs -> (head xs, length xs)) (group (sort (words xs)))

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs = xs : tails' (tail xs)

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails' haystack)

encode :: Int -> String -> String
encode offset = map (\c -> chr $ ord c + offset)

decode :: Int -> String -> String
decode offset = map (\c -> chr $ ord c - offset)

decode' :: Int -> String -> String
decode' shift = encode (negate shift)

digitSum :: Int -> Int
digitSum = foldl (\acc x -> digitToInt x + acc) 0 . show

digitSum' :: Int -> Int
digitSum' = sum . map digitToInt . show

firstTo40 = find (\x -> digitSum x == 40) [1 ..]

firstTo n = find (\x -> digitSum x == n) [1 ..]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v) : xs)
  | key == k = Just v
  | otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key = foldl (\acc (k, v) -> if key == k then Just v else acc) Nothing

phoneBook :: Map.Map String String
phoneBook =
  Map.fromList
    [ ("betty", "111-1111"),
      ("betty", "111-1112"),
      ("bonnie", "222-2222"),
      ("patsy", "333-3331"),
      ("patsy", "333-3332"),
      ("patsy", "333-3333"),
      ("lucille", "444-4444"),
      ("wendy", "555-5555")
    ]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

intBook = Map.map string2digits phoneBook

phoneBook' =
  [ ("betty", "555-2938"),
    ("betty", "342-2492"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("patsy", "943-2929"),
    ("patsy", "827-9162"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492"),
    ("penny", "555-2111")
  ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add
  where
    add number1 number2 = number1 ++ ", " ++ number2

newPhoneBookMap = phoneBookToMap phoneBook'
