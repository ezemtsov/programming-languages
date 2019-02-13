-- phoneExercise.hs
module PhoneExercise where

import Data.Char
import Data.List

-- Create a data structure that captures the phone layout above.
-- The data structure should be able to express enough of how the
-- layout works that you can use it to dictate the behavior of the
-- functions in the following exercises.

-- validButtons = "1234567890*#"
type Digit = Char
type PhoneLayout = String

-- Valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone [(Digit, PhoneLayout)] deriving (Eq, Show)

daPhone :: DaPhone
daPhone = DaPhone
  [('1', "1"),
   ('2', "abc2"),
   ('3', "def3"),
   ('4', "ghi4"),
   ('5', "jkl5"),
   ('6', "mno6"),
   ('7', "pqrs7"),
   ('8', "tuv8"),
   ('9', "wxyz9"),
   ('*', "*^"),
   ('0', " +_0"),
   ('#', ".,#")]

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps (DaPhone phone) letter
  | isUpper letter = [('*',1)] ++ (result $ toLower letter)
  | otherwise = result letter
  where
    result l = [(fst x, countTaps l $ snd x) | x <- chosenDigit]
      where chosenDigit = filter (\x -> elem l $ snd x) phone
            countTaps c xs = go c xs 1
              where go c (x:xs) pos
                      | c == x = pos
                      | c /= x = go c xs pos+1
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead layout str =
  concat $ map (\x -> reverseTaps layout x) str

-- Convert the following conversations into the keypresses required
-- to express them. We're going to suggest types and functions to
-- fill in order to accomplish the goal, but they're not obligatory.
-- If you want to do it differently, go right ahead.

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha, Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

-- How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map (\(d,p) -> p)

-- What was the most popular letter for each message? What was
-- its cost? You'll want to combine reverseTaps and fingerTaps
-- to figure out what it cost in taps. reverseTaps is a list
-- because you need to press a different button in order to get
-- capitals
deduplicate :: Eq a => [a] -> [a]
deduplicate [] = []
deduplicate (x:xs) =
  if (elem x xs)
  then deduplicate xs
  else x:deduplicate xs

index :: Eq a => [a] -> [(Int, a)]
index str = applyTo $ deduplicate str
  where applyTo xs = [(count x str, x) | x <- xs]
        count c str@(x:_) =
          foldl (\base x -> if (x == c)
                            then (base+1)
                            else base) 0 str

maxIndex :: Eq a => [(Int, a)] -> Int
maxIndex =
    maximum
  . map (\(i,v) -> i)

mostPopularLetter :: String -> [(Int,Char)]
mostPopularLetter str =
    filter (\(l,r) -> l == maxFromIndex)
  . index
  $ str
  where maxFromIndex =
            maxIndex
          . index
          $ str

count :: Eq a => a -> [a] -> Int
count e list =
    sum
  $ map (\(i, v) ->
         if (v == e)
         then i
         else 0)
  $ (index list)

costOfLetter :: Char -> String -> Int
costOfLetter c str = tapWeight * occurencies
  where
    occurencies = count c str
    tapWeight =
        sum
      . map (\(d,p) -> p)
      $ (reverseTaps daPhone c)

-- What was the most popular letter overall?
-- What was the most popular word?
coolestLtr :: [String] -> [(Int,Char)]
coolestLtr =
    mostPopularLetter
  . concat
  . intersperse ". "

coolestWord :: [String] -> [String]
coolestWord input =
    map (\(i,v) -> v) 
  . filter (\(i,v) -> i == maxIndex wordSet)
  $ wordSet
  where wordSet =
            index
          . words
          . concat
          . intersperse " "
          $ input
