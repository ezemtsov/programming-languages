

{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

data FixMePls a b =
    FixMe
  | Pls a b
  deriving (Eq, Show)

instance Functor (FixMePls a) where
  fmap _ FixMe = FixMe
  fmap f (Pls a b) = Pls a (f b)

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

-- instance Functor WhoCares where
--   fmap _ ItDoesnt = ItDoesnt
--   fmap _ WhatThisIsCalled =
--     WhatThisIsCalled
--   fmap f (Matter a) = Matter (f a)

instance Functor WhoCares where
  fmap _ ItDoesnt = WhatThisIsCalled
  fmap f WhatThisIsCalled = ItDoesnt
  fmap f (Matter a) = Matter (f a)

-- data CountingBad a =
--   Heisenberg Int a
--   deriving (Eq, Show)

-- instance Functor CountingBad where
--   fmap f (Heisenberg n a) =
--     Heisenberg (n+1) (f a)

-- instance Functor CountingBad where
--   fmap f (Heisenberg n a) =
--     Heisenberg (n+1) (f a)
    
data CountingGood a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg n a) =
    Heisenberg (n) (f a)

--------------------------------------------------

data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a)  where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

--------------------------------------------------
-- QuickCheck properties for Functor
--------------------------------------------------

functorIdentity :: (Functor f, Eq (f a)) =>
                      f a
                   -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                     (a -> b)
                  -> (b -> c)
                  -> f a
                  -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                      f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

--------------------------------------------------
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a
           => Maybe a
           -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a
          => Maybe a
          -> Maybe String
showMaybe s = fmap show s

incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a
            => Maybe a
            -> Maybe String
showMaybe'' = fmap show

liftedInc :: (Functor f, Num b)
          => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a)
           => f a -> f String
liftedShow = fmap show

--------------------------------------------------

incIfRight :: Num a
           => Either e a
           -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e)= Left e

showIfRight :: Show a
            => Either e a
            -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

incEither :: Num a
          => Either e a
          -> Either e a
incEither m = fmap (+1) m

showEither :: Show a
           => Either e a
           -> Either e String
showEither s = fmap show s

incEither' :: Num a
           => Either e a
           -> Either e a
incEither' = fmap (+1)

showEither' :: Show a
            => Either e a
            -> Either e String
showEither' = fmap show

--------------------------------------------------

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

--------------------------------------------------

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f
      => Functor (Wrap f) where
  fmap f (Wrap a) = Wrap (fmap f a)

--------------------------------------------------
-- IO Functor

getInt :: IO Int
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ "and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)
