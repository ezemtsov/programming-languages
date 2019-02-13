-- signalingAdversity
module SignalingAdversity where

import Data.List

-- Smart constructors for datatypes
type Name = String
type Age = Integer
type ValidatePerson a =
  Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageOkay :: Age
        -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True  -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name
         -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True  -> Right name
  False -> Left [NameEmpty]


-- mkPerson :: Name
--          -> Age
--          -> Either PersonInvalid Person
-- mkPerson name age =
--   mkPerson' (nameOkay name) (ageOkay age)

-- mkPerson' :: ValidatePerson Name
--           -> ValidatePerson Age
--           -> ValidatePerson Person
-- mkPerson' (Right nameOk) (Right ageOk) =
--   Right (Person nameOk ageOk)
-- mkPerson' (Left badName) (Left badAge) =
--   Left (badName ++ badAge)
-- mkPerson' (Left badName) _ = Left badName
-- mkPerson' _ (Left badAge) = Left badAge

--------------------------------------------------

notThe :: String -> Maybe String
notThe str
  | str == "the" = Nothing
  | otherwise = Just str

replaceThe :: String -> String
replaceThe str
  | elem "the" $ wordList =
        concat
      . intersperse " "
      . tail
      $ wordList
  | otherwise = str
  where wordList = words str

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel str = go 0 (words str)
  where go count (x:xs)
          | xs == [] = count
          | x == "the" && elem (head $ head xs) "aeiouAEIOU"
          = go (count+1) xs
          | otherwise = go count xs

countVowels :: String -> Integer
countVowels =
    toInteger
  . length
  . filter vowels
  where vowels x = elem x "aeiouAEIOU"

--------------------------------------------------

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str
  | vowelsCount > consonantCount = Nothing
  | otherwise = Just (Word' str)
  where
    vowelsCount =
        length
      . filter (\x -> elem x vowels)
      $ str
    consonantCount =
      length str - vowelsCount

--------------------------------------------------

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = natToInteger a + 1

integerToNat :: Integer -> Maybe Nat
integerToNat a
  | a < 0  = Nothing
  | otherwise = Just (convert a)
  where convert a
          | a == 0 = Zero
          | otherwise = Succ (convert (a-1))

--------------------------------------------------

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just a) = False

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b f (Just a) = f a
mayybe b f Nothing = b

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback Nothing = fallback
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes =
    map (\(Just a) -> a)
  . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if (length $ filter isNothing xs) > 0
  then Nothing
  else Just (catMaybes xs)

--------------------------------------------------

lefts' :: [Either a b] -> [a]
lefts' = foldr onlyLeft []
  where
    onlyLeft x base = case x of
      Left a -> a:base
      _      -> base

rights' :: [Either a b] -> [b]
rights' = foldr onlyRight []
  where
    onlyRight x base = case x of
      Right b -> b:base
      _       -> base

partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' xs =
  (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' f _ = Nothing

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' a2c b2c x = case x of
  Left a -> a2c a
  Right b -> b2c b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f x =
  either'
  (\x -> Nothing)
  (\x -> Just (f x))
  x

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n+x) xs)

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go xs' [] = xs'
        go xs' (x:xs) = (go (xs' ++ x) xs)

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

--------------------------------------------------

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x =
  myUnfoldr (\x -> Just (x, f x)) x

--------------------------------------------------

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (l,v,r) -> Node (unfold f l) v (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold
  (\x -> if (x==n) then Nothing else (Just (x+1,x,x+1)))
  0
