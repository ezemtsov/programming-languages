{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (Result (5, 0))
    it "22 divided by 5 is \
       \4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (Result (4, 2))
    it "7 multiplied by 5 is 35" $ do
      multipliedBy 7 5 `shouldBe` 35
    it "42 multipliedBy 8 is 336" $ do
      multipliedBy 42 8 `shouldBe` 336
    it "x + 1 is always\
       \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

data DividedResult =
    Result (Integer, Integer)
  | DividedByZero 
  deriving (Eq, Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = Result (count, n)
         | otherwise =
             go (n - d) d (count + 1)

multipliedBy :: (Eq a, Num a) => a -> a -> a
multipliedBy _ 0 = 0
multipliedBy 0 _ = 0
multipliedBy m b = m + multipliedBy m (b - 1)

--------------------------------------------------

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b,
                Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b)
          => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
