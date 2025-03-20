multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]

-- zipWith' (zipWith' (*)) [[1, 2, 3], [3, 5, 6], [2, 3, 4]] [[3, 2, 3], [3, 4, 5], [5, 4, 3]]
-- [[3,4,9],[9,20,30],[10,12,12]]

flip1 :: (a -> b -> c) -> b -> a -> c
flip1 f = g
  where
    g x y = f y x

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f y x = f x y

map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x : xs) = f x : map1 f xs

-- map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
-- [[1,4],[9,16,25,36],[49,64]]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- let notNull x = not (null x) in filter notNull [[1, 2, 3], [], [3, 4, 5], [2, 2], [], []]

quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (x : xs) = quicksort1 smaller ++ [x] ++ quicksort1 largerOrEqual
  where
    largerOrEqual = filter (>= x) xs
    smaller = filter (< x) xs

quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 (x : xs) =
  let largerOrEqual = filter (>= x) xs
      smaller = filter (< x) xs
   in quicksort2 smaller ++ [x] ++ quicksort2 largerOrEqual

largestDivisible :: Int
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

sumOfOddSquares :: Integer
sumOfOddSquares = sum (map (^ 2) (filter odd (takeWhile (< 10000) [1, 2 ..])))

sumOfOddSquares' = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz x
  | even x = x : collatz (x `div` 2)
  | odd x = x : collatz (x * 3 + 1)

collatzCountAbove15 :: Int
collatzCountAbove15 = length (filter p (map collatz [1 .. 100]))
  where
    p xs = length xs > 15

nxList :: (Num a) => a -> [a] -> [a]
-- nxList n xs = map (n *) xs
nxList n = map (n *)

listOfFuns = map (*) [0 ..]

-- lambda ------------------------------------------------------------
collatzCountAbove15' :: Int
collatzCountAbove15' = length (filter (\xs -> length xs > 15) (map collatz [1 .. 100]))

customZipWith = zipWith (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]

-- patternMatchLambda1 = map (\(a, b) -> a + b) [(1, 2), (3, 5), (6, 3), (2, 6), (2, 5)]
patternMatchLambda2 = map (uncurry (+)) [(1, 2), (3, 5), (6, 3), (2, 6), (2, 5)]

addThree1 x y z = x + y + z

-- addThree2 = \x -> \y -> \z -> x + y + z
-- addThree3 = \x y z -> x + y + z

-- flip4 :: (a -> b -> c) -> b -> a -> c
-- flip4 f = \x y -> f y x

textGen = zipWith (flip (++)) ["love you", "love me"] ["i ", "you "]

subtra = map (subtract 20) [1, 2, 3, 4]

-- fold --------------------------------------------------------

sum1 :: (Num a) => [a] -> a
sum1 xs = foldl (\acc x -> acc + x) 0 xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x ys = foldr (\y acc -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse1 :: [a] -> [a]
reverse1 = foldl (\acc x -> x : acc) []

reverse2 :: [a] -> [a]
reverse2 = foldl (flip (:)) []

product1 :: (Num a) => [a] -> a
product1 = foldl (*) 1

filter1 :: (Foldable t) => (a -> Bool) -> t a -> [a]
filter1 p = foldr (\x acc -> if p x then x : acc else acc) []

last1 :: [a] -> a
last1 = foldl1 (\_ x -> x)

rev = flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6

-- (&&) :: Bool -> Bool -> Bool
-- True && x = x
-- False && _ = False

and1 :: [Bool] -> Bool
and1 = foldr (&&) True

scanlList = scanl (+) 0 [3, 5, 2, 1] -- [0, 3, 8, 10, 11]

scanrList = scanr (+) 0 [3, 5, 2, 1] -- [11, 10, 8, 3, 0]

-- [3, 4, 5, 6, 7, 9, 2, 1] >> [3, 4, 5, 6, 7, 9, 9, 9]
scanl1List = scanl1 max [3, 4, 5, 6, 7, 9, 2, 1]

adsf = scanl (flip (:)) [] [3, 2, 1]

-- 自然数の平方根を小さいものから足して行った時、1000を超えるのは何個め？
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- 1,2 ..を順番に足して行った時、5555を超えるのは何個目？
ans = length (takeWhile (< 5555) (scanl1 (+) [1 ..])) + 1

-- $ を使った関数適用 --------------------------------------------------------

ans1 = sum (map sqrt [1 .. 130])

ans2 = sum $ map sqrt [1 .. 130]

ans3 = sqrt (3 + 4 + 9)

ans4 = sqrt $ 3 + 4 + 9

ans5 = sum (filter (> 10) (map (* 2) [2 .. 10]))

ans6 = sum $ filter (> 10) (map (* 2) [2 .. 10])

ans7 = sum $ filter (> 10) $ map (* 2) [2 .. 10]

funcApplication = map ($ 3) [(4 +), (10 *), (^ 2), sqrt]

-- 関数合成 ------------------------------------------------------------------
fn = negate . (* 3)

aa = map (\x -> negate (abs x)) [5, -3, -6, 7, -3]

bb = map (negate . abs) [5, -3, -6, 7, -3]

cc = map (\xs -> negate (sum (tail xs))) [[1 .. 5], [3 .. 6], [1 .. 7]]

dd = map (negate . sum . tail) [[1 .. 5], [3 .. 6], [1 .. 7]]

-- 他引数関数の関数合成
e1 = sum (replicate 5 (max 6.7 8.9))

e2 = (sum . replicate 5) (max 6.7 8.9)

e3 = sum . replicate 5 $ max 6.7 8.9

f1 = replicate 2 (product (map (* 3) (zipWith max [1, 2] [4, 5])))

f2 = (replicate 2 . product . map (* 3)) $ zipWith max [1, 2] [4, 5]

-- ポイントフリースタイル ------------------------------------------------------
func1 x = ceiling (negate (tan (cos (max 50 x))))

func1' = ceiling . negate . tan . cos . max 50

res1 :: Integer
res1 = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

res2 :: Integer
res2 = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]
