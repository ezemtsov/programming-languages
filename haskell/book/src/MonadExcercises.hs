import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Monad

--------------------------------------------------
-- Nope Monad

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg
  return _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

--------------------------------------------------
-- PhhhbbttEither Monad

data PhhhbbttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b

instance Applicative (PhhhbbttEither b) where
  pure a = Left' a
  Left' a1 <*> Left' a2 = Left' $ a1 a2
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b

instance Monad (PhhhbbttEither b) where
  Left' a >>= f = f a
  Right' b >>= _ = Right' b

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (PhhhbbttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Left' a)
              ,(1, return $ Right' b)]

instance (Eq b, Eq a) => EqProp (PhhhbbttEither b a) where
  (=-=) = eq

--------------------------------------------------
--- Identity Monad

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity b) = Identity $ f b

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a
      => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a
      => EqProp (Identity a) where
  (=-=) = eq

--------------------------------------------------
--- List Monad

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> a = a
  a <> Nil = a
  Cons a l <> l' = Cons a (l <> l')

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure a = Cons a Nil
  
  Nil <*> al = Nil
  Cons f fl <*> al =
    (f <$> al) <> (fl <*> al)

instance Monad List where
  return = pure

  Nil >>= _ = Nil
  Cons a l >>= f = f a <> (l >>= f)

instance Arbitrary a
      => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    frequency [(1, return $ Nil)
              ,(5, return $ Cons a l)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

check :: Gen (List Integer)
check = do
  x <- arbitrary
  return x

main = do
  let tNope = undefined :: Nope (Int, String, Int)
  let tPhhhbbttEither = undefined :: PhhhbbttEither Int (Int, String, Int)
  let tIdentity = undefined :: Identity (Int, String, Int)
  let tList = undefined :: List (Int, String, Int)
  putStrLn "\n Nope type tests:"
  quickBatch (functor tNope)
  quickBatch (applicative tNope)
  quickBatch (monad tNope)
  putStrLn "\n PhhhbbttEither type tests:"
  quickBatch (functor tPhhhbbttEither)
  quickBatch (applicative tPhhhbbttEither)
  quickBatch (monad tPhhhbbttEither)
  putStrLn "\n tIdentity type tests:"
  quickBatch (functor tIdentity)
  quickBatch (applicative tIdentity)
  quickBatch (monad tIdentity)
  putStrLn "\n tList type tests:"
  quickBatch (functor tList)
  quickBatch (applicative tList)
  quickBatch (monad tList)

--------------------------------------------------
--------------------------------------------------
-- Write the following functions using the methods
-- provided by Monad and Functor. Using stuff like
-- identity and composition is fine, but it has to
-- typecheck with types provided

j :: Monad m => m (m a) -> m a
j m = m >>= id
-- join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = m >>= (return . f)
-- fmap

l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb =
  ma >>=
  \a -> mb >>=
  \b -> return $ f a b
-- l2 f ma mb = do
--   a <- ma
--   b <- mb
--   return $ f a b

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf =
  ma >>=
  \a -> mf >>=
  \f -> return $ f a
--a = flip (<*>)

-- [1,2,3] -> (\x -> Just x) -> Just "123"
-- [Just '1', Just '2', Just '3']
-- Just "123"

meh :: Monad m
   => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (a:as) f =
  f a >>=
  \a -> meh as f >>=
  \lb -> return $ a:lb
-- meh (x:xs) f = do
--   v <- f x
--   l <- meh xs f
--   return $ v:l

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

kleisli :: Monad m
        => (a -> m b) -> (b -> m c) -> a -> m c
kleisli fa fb a =
  fa a >>=
  \b -> fb b >>=
  \c -> return c
