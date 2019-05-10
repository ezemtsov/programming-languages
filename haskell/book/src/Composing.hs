{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA2)
import Control.Monad (join)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f =>
         Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h)
      => Functor (Three f g h) where
  fmap f (Three fgha) =
    Three $ (fmap . fmap . fmap) f fgha

instance (Applicative f, Applicative g)
       => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ liftA2 (<*>) f a
--    Compose $ (<*>) <$> f <*> a

instance (Monad f, Monad g)
       => Monad (Compose f g) where
  return = pure

  (>>=) :: Compose f g a
        -> (a -> Compose f g b)
        -> Compose f g b
  (>>=) f g = undefined  -- doesn' work

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap f g = foldMap (foldMap f) . getCompose $ g

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse f (Compose g) = Compose <$>
    (sequenceA $ sequenceA . fmap f <$> g)

--------------------------------------------------
-- It’s a functor that can map over two type
-- arguments instead of one:

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- Write Bifunctor instances for the following types:

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d =
  Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b =
    Left' a
  | Right' b

instance Bifunctor Either' where
  bimap f g (Left' a) = Left' (f a)
  bimap f g (Right' b) = Right' (g b)

--------------------------------------------------
-- Introduction to MonadT

-- We can create a type wrapper for each combination
-- of types to get a monad, but we won't.

newtype MaybeIO a =
  MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a =
  MaybeList { runMaybeList :: [Maybe a] }


-- Now thee proper way:

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m)
      => Functor (IdentityT m) where
  fmap f (IdentityT fa) =
    IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) =
    Identity (f a)

instance (Applicative m)
      => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a

instance (Monad m)
      => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f

--------------------------------------------------

innerMost
  :: [Maybe (Identity (a -> b))]
  -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second'
  :: [Maybe (Identity a -> Identity b)]
  -> [ Maybe (Identity a)
     -> Maybe (Identity b)]
second' = fmap (<*>)

final'
  :: [ Maybe (Identity a)
     -> Maybe (Identity b) ]
  -> [Maybe (Identity a)]
  -> [Maybe (Identity b)]
final' = (<*>)

lmiApply :: [Maybe (Identity (a -> b))]
         -> [Maybe (Identity a)]
         -> [Maybe (Identity b)]
lmiApply f x =
  final' (second' (innerMost f)) x
