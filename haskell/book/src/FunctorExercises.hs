{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module FunctorExercises where

import Test.QuickCheck
import GHC.Arr

functorIdentity :: (Functor f, Eq (f a)) =>
                   f a
                -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                  (a -> b)
               -> (b -> c) -> f a
               -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)


a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap.fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = fmap (*2) (\x -> x - 2)
d = fmap
    ((return '1' ++) . show)
    (\x -> [x, 1..3])
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++)) $ fmap show ioi
     in fmap (*3) changed

--------------------------------------------------
-- Instances of Func
--------------------------------------------------

-- Implement Functor instances for the following datatypes.
-- Use the QuickCheck properties we showed you to validate them

newtype Identity a = Identity a
  deriving (Eq, Show)

data Pair a = Pair a a
  deriving (Eq, Show)

data Two a b = Two a b
  deriving (Eq, Show)

data Three a b c = Three a b c
  deriving (Eq, Show)

data Three' a b = Three' a b b
  deriving (Eq, Show)

data Four a b c d = Four a b c d
  deriving (Eq, Show)

data Four' a b = Four' a a a b
  deriving (Eq, Show)

data Trivial = Trivial
-- doing trivial is impossible since we need a higher kinded type

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x
  
--------------------------------------------------
-- Exercise: Possibly

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ (LolNope) = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

--------------------------------------------------
-- Exercise: Either

-- data Sum a b =
--     First a
--   | Second b
--   deriving (Eq, Show)

-- instance Functor (Sum a) where
--   fmap f (First a) = First a
--   fmap f (Second b)= Second (f b)

--------------------------------------------------
--------------------------------------------------
--------------------------------------------------

-- Last excercises


--------------------------------------------------

-- Determine if a valid Functor can be written for the datatype provided

-- 1 Not possible since we need a higher kinded type
-- data Bool =
--   False | True
--   deriving (Eq, Show)

-- 2
data BoolAndSomethingElse a =
  False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- 3
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ (Falsish) = Falsish
  fmap f (Truish a) = Truish (f a)

-- 4 Use the kinds to guide you on this one,
--   don't get too hung on the details

newtype Mu f = InF { outF :: f (Mu f) }

-- Probably not, because Mu has kind of (* -> *) -> *

-- 5 Not possible D has kind *
data D =
  D (Array Word Word) Int Int

--------------------------------------------------
-- Rearrange the arguments to the type constructor of
-- the datatype so the Functor instance works.

-- 1
data Sum a b =
    First a
  | Second b

instance Functor (Sum e) where
   fmap f (First a) = First a
   fmap f (Second b) = Second (f b)

-- 2
data Company a b c =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  fmap f (Something b) = Something b

-- 3
data More a b =
    L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--------------------------------------------------
-- Write Functor instances for the following datatypes

-- 1
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a
  deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' (f a)

-- 4
data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f
      => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6
data Parappa f g a = DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g)
       => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g
      => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) =
    IgnoringSomething fa (fmap f gb)

-- 8
data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g
      => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) =
    Cons (f a) (fmap f b)

-- 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) =
    MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read s') = Read (f . s')
