module WordNumberTest where

import Test.Hspec
import Test.QuickCheck
import WordNumber
  (digitToWord, digits, wordNumber)
import Data.List (sort)
import Data.Char

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100
        `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe`
        "nine-zero-zero-one"
        
  describe "halfIdentity" $ do
    it "x * 2 / 2 is always equal x" $ do
      property $ \x -> halfIdentity x == (x :: Double)

  describe "listOrdered" $ do
    it "sorted list is always ordered" $ do
      property $ \x -> listOrdered $ sort (x :: [Char])

  describe "plusAssociative" $ do
    it "testing associative property of sum" $ do
      property $ \x y z -> plusAssociative
                          (x :: Integer)
                          (y :: Integer)
                          (z :: Integer)

  describe "plusCommutative" $ do
    it "testing commutative property of sum" $ do
      property $ \x y -> plusCommutative
                         (x :: Integer)
                         (y :: Integer)


  describe "multiplyAssociative" $ do
    it "testing associative property of sum" $ do
      property $ \x y z -> multiplyAssociative
                          (x :: Integer)
                          (y :: Integer)
                          (z :: Integer)

  describe "multiplyCommutative" $ do
    it "testing commutative property of sum" $ do
      property $ \x y -> multiplyCommutative
                         (x :: Integer)
                         (y :: Integer)

  describe "prop_quotRem" $ do
    it "testing quot rem property" $ do
      property $ \x y -> prop_quotRem
                           (x :: Integer)
                           (y :: Integer)

  describe "prop_divMod" $ do
    it "testing div mod property" $ do
      property $ \x y -> prop_divMod
                         (x :: Integer)
                         (y :: Integer)

-- 1. for a function
half x = x / 2
--   this property should hold
halfIdentity = (*2) . half

-- 2. for any list you apply sort to
--    this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative x y =
  x + y == y + x

multiplyAssociative x y z =
  x * (y * z) == (x * y) * z
multiplyCommutative x y =
  x * y == y * x

prop_quotRem _ 0 = True
prop_quotRem x y =
  (quot x y)*y + (rem x y) == x

prop_divMod _ 0 = True
prop_divMod x y =
  (div x y)*y + (mod x y) == x

-- ^ is not commutative neither associative
powerAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z
powerCommutative x y =
  x ^ y == y ^ x

-- this should be correct
prop_reverse xs =
  (reverse $ reverse xs) == id xs

-- write a property for the definition of $
prop_dollar f a =
  (f $ a) == (f a)

prop_dot f g a =
  (f . g) a == f (g a)

-- see if these two functions are equal (they are)
prop_semicolon l b = 
  foldr (:) b l == (++) b l

prop_concat a =
  foldr (++) [] a == concat a

-- hm. is that so? (no, you can take more values than list has)
f n xs = length (take n xs) == n

-- final one
f' x = (read (show x)) == x

-- (this one works with Num and Eq constrains, can be checked against x^2)
square :: (Num a, Eq a) => a -> a
square x = x * x

-- (this one does not work beacuse sqrt is Float based and floats are poop)
squareIdentity = square . sqrt

--------------------------------------------------
-- Idempotence (wtf is that??)

twice f = f . f
fourTimes = twice . twice

-- 1
f1 x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

-- 2
f2 x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) 
  | x == ' '  = capitalizeWord xs
  | otherwise = toUpper x : xs 

--------------------------------------------------
-- Make a Gen random generator for the datatype

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

-- 1 Equal probabilities for each

foolGen :: Gen Fool
foolGen = do
  elements [Fulse, Frue]

-- 2 2/3s chance of Fulse, 1/3 chance of Frue

foolGen' :: Gen Fool
foolGen' = do
  frequency [(2, return $ Fulse),
             (1, return $ Frue)]

instance Arbitrary Fool where
  arbitrary = foolGen'
