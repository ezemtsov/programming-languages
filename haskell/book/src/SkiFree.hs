{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary


instance ( Applicative n
         , Testable (n Property)
         , EqProp a )
        => EqProp (S n a) where
  (S x y) =-= (S p q) =
        (property $ (=-=) <$> x <*> p)
    .&. (y =-= q)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n
      => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

type TI = S []

main = do
  sample' (arbitrary :: Gen (S [] Int))
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)
