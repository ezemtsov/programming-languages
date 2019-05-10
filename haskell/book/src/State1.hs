{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.State hiding (get, put, modify)

-- newtype State s a =
--   State { runState :: s -> (a, s) }
  
--------------------------------------------------

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $
    \s -> let (a, s') = g s
    in (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $
    \s -> let (h, s') = f s
              (a, s'') = g s
    in (h a, s)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $
    \s -> let (a, s') = f s
    in runMoi (g a) s'

--------------------------------------------------
-- Chapter exercises

-- Construct a State where the state is also the value
-- you return.

get :: State s s
get = state $ \x -> (,) x x

-- Construct a State where the resulting state is the
-- argument provided and the value is defaulted to unit

put :: s -> State s ()
put s = state $ \x -> ((), s)

-- Run the State with s and get the state that results.

exec :: State s a -> s -> s
exec sa s = snd $ runState sa s

-- Run the State with s and get the value that results.

eval :: State s a -> s -> a
eval sa = fst . runState sa

-- Write a function which applies a function to crete a
-- new State

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), (f s))
