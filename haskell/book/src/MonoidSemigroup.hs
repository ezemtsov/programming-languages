module MonoidSemigroup where

import Madness
import Test.QuickCheck
import Data.List.NonEmpty
import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada = Nada
  Only a <> Nada = Only a
  Nada <> Only a = Only a
  Only a <> Only b = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)

--------------------------------------------------
-- Ex: Maybe another monoid
--------------------------------------------------

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where
  a <> _ = a

instance Monoid a => Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (2, return $ First' (Only a))
              , (1, return $ First' Nada)]

firstMappend :: Monoid a
             => First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  verboseCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

--------------------------------------------------
-- Semigroup
--------------------------------------------------

-- Trivial type

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool


-- Identity type

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
    
type IdAssoc =
  Identity String -> Identity String -> Identity String -> Bool


-- Two type

data Two a b = Two a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a,
          Monoid b) =>
         Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

type TwoAssoc =
  (Two String String) -> (Two String String) -> (Two String String) -> Bool


-- Three type

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Semigroup a,
          Semigroup b,
          Semigroup c) =>
         Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) =
    Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Monoid a,
          Monoid b,
          Monoid c) =>
         Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

type ThreeAssoc =
  (Three String String String) ->
  (Three String String String) ->
  (Three String String String) -> Bool

-- Four type

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c,
          Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Semigroup a,
          Semigroup b,
          Semigroup c,
          Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) =
    Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)
    
instance (Monoid a,
          Monoid b,
          Monoid c,
          Monoid d) =>
         Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)

type FourAssoc =
  (Four String String String String) ->
  (Four String String String String) ->
  (Four String String String String) -> Bool

-- BoolConj type

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _) <> (BoolConj _) = BoolConj False
    
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool


-- BoolDisj type

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj _) = BoolDisj True
  (BoolDisj _) <> (BoolDisj True) = BoolDisj True
  (BoolDisj _) <> (BoolDisj _) = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool


-- Or type

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a,
          Arbitrary b) =>
         Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a)
              ,(1, return $ Snd b)]

instance Semigroup (Or a b) where
  Fst a <> Snd b = Snd b
  Fst a <> Fst b = Fst a
  Snd a <> Fst b = Snd a
  Snd a <> Snd b = Snd a

type OrAssoc =
  (Or String String) ->
  (Or String String) ->
  (Or String String) -> Bool

-- Combine a b

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (<>) a b = Combine $ (<>) (unCombine a) (unCombine b)

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

propCombine :: Integer -> Bool
propCombine x =
  (2 * x) == (getSum . unCombine (f <> g) $ x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    a <- arbitrary
    return $ Combine a

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- How to check it with quickCheck? You need Show and Eq, but how to write it?


-- Comp a

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  a <> b = Comp $ (<>) (unComp a) (unComp b)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    a <- arbitrary
    return $ Comp a
-- How to check it with quickCheck? You need Show and Eq, but how to write it?

propComp :: String -> Bool
propComp s =
  s == unComp (Comp id) s

-- Validaion a b

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
  (<>) (Success' a) (Failure' b) = Success' a
  (<>) (Failure' a) (Failure' b) = Failure' (a <> b)
  (<>) (Success' a) (Success' b) = Success' a
  (<>) (Failure' a) (Success' b) = Success' b


-- Associativity Property and main

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

main' :: IO ()
main' = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)  
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: Three String String String -> Bool)
  quickCheck (monoidRightIdentity :: Three String String String -> Bool)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: Four String String String String -> Bool)
  quickCheck (monoidRightIdentity :: Four String String String String -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck propCombine
  quickCheck propComp


main'' = do
  let failure :: String
              -> Validation String Int
      failure = Failure'
      success :: Int
             -> Validation String Int
      success = Success'
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2


-- Mem
newtype Mem s a =
  Mem { runMem :: s -> (a,s)}

instance Semigroup a => Semigroup (Mem s a) where
  (<>) a b = Mem $ merge (runMem a) (runMem b)
    where merge f1 f2 x = (left, right)
            where left = (fst $ f1 x) <> (fst $ f2 x)
                  right = snd . f2 . snd . f1 $ x

instance Monoid a => Monoid (Mem s a) where
   mempty = Mem (\x -> (mempty, x))
   mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

main''' = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
