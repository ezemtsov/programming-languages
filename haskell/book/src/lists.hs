-- lists.hs
module Lists where

myTail :: [a] -> [a]
myTail [] = []
myTail (_ : xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

eftBool :: Bool -> Bool -> [Bool]
eftBool a b = go a b []
  where go start current result
         | current < start = result
         | current == start = current:result
         | otherwise = go start (pred current) (current:result)
  
eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd a b = go a b []
  where go start current result
         | current < start = result
         | current == start = current:result
         | otherwise = go start (pred current) (current:result)

eftInt :: Int -> Int -> [Int]
eftInt a b = go a b []
  where go start current result
         | current < start = result
         | current == start = current:result
         | otherwise = go start (pred current) (current:result)

eftChar :: Char -> Char -> [Char]
eftChar a b = go a b []
  where go start current result
         | current < start = result
         | current == start = current:result
         | otherwise = go start (pred current) (current:result)

myWords :: String -> Char -> [String]
myWords s sc = go s sc []
  where go str sc result
         | str == "" = result
         | otherwise = go
                        (dropWhile (== sc) (dropWhile (/= sc) str))
                        sc
                        (result ++ [(takeWhile (/= sc) str)])
