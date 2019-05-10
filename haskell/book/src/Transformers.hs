{-# LANGUAGE InstanceSigs #-}

module Transformers where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype IdentityT m a =
  IdentityT { runIdentityT :: m a }

instance Functor m
      => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT (f <$> ma)

instance Applicative m
      => Applicative (IdentityT m) where
  pure = IdentityT . pure

  (IdentityT mab) <*> (IdentityT ma) =
    IdentityT (mab <*> ma)

instance Monad m
      => Monad (IdentityT m) where
  return = pure

  (IdentityT ma) >>= f =
    IdentityT (ma >>= runIdentityT . f)

--------------------------------------------------

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m)
      => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m)
       => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m)
      => Monad (MaybeT m) where
  return = pure

  (MaybeT ma) >>= f =
    MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just a -> runMaybeT (f a)

--------------------------------------------------

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT mea) =
    EitherT $ (fmap . fmap) f mea

instance Applicative m
      => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))
  (EitherT fab) <*> (EitherT mea) =
    EitherT $ (<*>) <$> fab <*> mea

instance Monad m
      => Monad (EitherT e m) where
  return = pure

  (EitherT mea) >>= f = EitherT $ do
    v <- mea
    case v of
      Left e -> return (Left e)
      Right a -> runEitherT (f a)

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT mea) =
  EitherT $ swapEither <$> mea

swapEither :: Either e a
           -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> (EitherT a m b)
        -> m c
eitherT f g (EitherT mab) =
  mab >>= either f g

--------------------------------------------------

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m
      => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT (fmap (fmap f) rma)

instance Applicative m
      => Applicative (ReaderT r m) where
  pure a = ReaderT (\r -> pure a)

  (ReaderT rmab) <*> (ReaderT rma) =
    ReaderT ((<*>) <$> rmab <*> rma)

instance Monad m
      => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

--------------------------------------------------

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

instance Functor m
      => Functor (StateT s m) where
  fmap f (StateT sm) =
    StateT $ (fmap (fmap (mapFst f))) sm

instance Monad m
      => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  
  (StateT smab) <*> (StateT sma) =
    StateT $ \s -> do
--    this will not work as we need calculate state through each function
--    \s -> (flip (,) s) <$> ((fst <$> smab s) <*> (fst <$> sma s))
    (fab, s1) <- smab s
    (a, s2) <- sma s1
    return (fab a, s2)

instance Monad m
      => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f =
    StateT $ \s -> do
    (a, s) <- sma s
    runStateT (f a) s

--------------------------------------------------

instance MonadTrans MaybeT where
  lift ma = MaybeT (Just <$> ma)

instance MonadTrans (ReaderT r)  where
  lift ma = ReaderT (\r -> ma)

instance MonadTrans (EitherT e) where
  lift ma = EitherT (Right <$> ma)

instance MonadTrans (StateT s) where
  lift ma = StateT (\s -> (flip (,) s) <$> ma)

instance (MonadIO m)
       => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO

instance (MonadIO m)
      => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

instance (MonadIO m)
      => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m)
      => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m)
      => MonadIO (StateT s m) where
  liftIO = lift . liftIO


--------------------------------------------------
-- rDec is a function that should get its argument
-- in the context of Reader and return a value
-- decremented by one

newtype Identity a = Identity { runIdentity :: a }

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader (ReaderT ra) r = runIdentity (ra r)

rDec :: Num a => Reader a a
rDec = ReaderT (Identity . subtract 1)

rShow :: Show a
      => ReaderT a Identity String
rShow = ReaderT (Identity . show)

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT f
  where f a = do
          putStrLn ("Hi: " ++ show a)
          return $ a + 1

sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT f
  where f a = do
          putStrLn ("Hi: " ++ show a)
          return (show a, a + 1)
