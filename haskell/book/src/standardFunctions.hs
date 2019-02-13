-- standardFunctions.hs
module StandardFunctions where

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
  then False
  else myAnd xs

-- direct recursion, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd xs

-- fold, not point-free
-- in the folding function
myAnd'' :: [Bool] -> Bool
myAnd'' = foldr
          (\a b ->
             if a == False
             then False
             else b) True

-- fold, both myAnd and the folding
-- function are point-free now
myAnd''' :: [Bool] -> Bool
myAnd''' = foldr (&&) True

-- 1. myOrd returns True of any Bool in the list is True
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True
  then True
  else myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr
        (\a b ->
           if a == True
           then True
           else b) False

myOr'' :: [Bool] -> Bool
myOr'' = foldr (||) False

-- 2. myAny returns True if a -> Bool applied to
--    any of the values in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
  if f x == True
  then True
  else myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f xs =
  foldr (\x b -> f x || b) False xs

-- 3. myElem and myElem with any
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem c (x:xs) =
  if c == x
  then True
  else myElem c xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' c = any (== c)

myElem'' :: Eq a => a -> [a] -> Bool
myElem'' c = foldr
           (\a b ->
              if a == c
              then True
              else b) False

-- 4. Implement myReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- 5. Squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squish' :: [[a]] -> [a]
squish' = foldr (\a b -> a ++ b) []

-- 6. SquishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = squish' . myMap f

-- 7. squishAgain flattens a list of lists into a list.
--    This time re-use the squishMap function
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

squishAgain' :: [[a]] -> [a]
squishAgain' = squishMap' id
  
-- 8. myMaximumBy takes a comparison function and a list and returns
--    the greatest element of the list based on the last values that
--    the comparison returned GT
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Empty list"
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs)
  | f x (head xs) == GT = myMaximumBy f ([x] ++ tail xs)
  | otherwise = myMaximumBy f xs

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' _ [] = error "Empty list"
myMaximumBy' f xs = foldl maximize (head xs) xs
  where maximize a b = case f a b of
          GT -> a
          _  -> b

-- 9. myMinimumBy takes a comparison function and a list and returns
--    the least element of the list based on the last value that the
--    comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Empty list"
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs)
  | f x (head xs) == LT = myMinimumBy f ([x] ++ tail xs)
  | otherwise = myMinimumBy f xs

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' _ [] = error "Empty list"
myMinimumBy' f xs = foldl minimize (head xs) xs
  where minimize a b = case f a b of
          LT -> a
          _  -> b

-- 10. Using the myMinimumby and myMaximumby functions, write your
--     own versions of maximum and minimum.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

-- 11. myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> (f a):b) []

-- 12. myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b ->
                      if ((f a) == True)
                      then a:b
                      else b) []
