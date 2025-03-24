maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' tar [] = False
elem' tar (x : xs)
  | tar == x = True
  | otherwise = tar `elem'` xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
   in quicksort smallerOrEqual ++ [x] ++ quicksort larger

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
  quicksort' smallOrEqual ++ [x] ++ quicksort' larger
  where
    smallOrEqual = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

-- maximum'
-- replicate'
-- take'
-- reverse'
-- repeat'
-- zip'
-- elem'
-- quicksort'
