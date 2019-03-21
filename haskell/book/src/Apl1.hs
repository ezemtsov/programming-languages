module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Semigroup a
      => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)

instance Monoid a
      => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = (<>)

-- instance Arbitrary a
--       => Arbitrary (ZipList a) where
--   arbitrary = ZipList <$> arbitrary

instance Eq a
      => EqProp (ZipList a) where
  (=-=) = eq

--------------------------------------------------
-- Write an Applicative instance for List datatype

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) Nil ys = ys
  (<>) xs Nil = xs
  (<>) (Cons x xs) ys =
    Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
  pure a = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons x xs) ys =
    (<>) (fmap x ys) (xs <*> ys)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' i (Cons a b) =
  (Cons a Nil) <> (take' (i-1) b)
take' _ _ = error "Wrong argument"

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs


zipList :: List (a -> b) -> List a -> List b
zipList Nil _ = Nil
zipList _ Nil = Nil
zipList (Cons x xs) (Cons y ys) =
  Cons (x y) (zipList xs ys)

instance Applicative ZipList' where
  pure a = ZipList' Nil
  (<*>) (ZipList' xs) (ZipList' ys) = ZipList' $ zipList xs ys

--------------------------------------------------
--------------------------------------------------
-- Following the examples from the book:
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as =
  concat' $ fmap f as

toMyList :: Foldable f
         => f a -> List a
toMyList = foldr Cons Nil
--------------------------------------------------
--------------------------------------------------

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Apl1.Failure e) = Apl1.Failure e
  fmap f (Apl1.Success a) = Apl1.Success (f a)

instance Monoid e =>
         Applicative (Validation e) where
  pure = Apl1.Success
  
  (<*>) (Apl1.Success a1) (Apl1.Success a2) =
    Apl1.Success (a1 a2)
  (<*>) (Apl1.Failure e1) (Apl1.Failure e2) =
    Apl1.Failure (e1 <> e2)
  (<*>) _ (Apl1.Failure e) = Apl1.Failure e
  (<*>) (Apl1.Failure e) _ = Apl1.Failure e
