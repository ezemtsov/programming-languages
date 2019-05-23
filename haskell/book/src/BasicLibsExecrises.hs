module Main where

import Criterion.Main
import qualified Data.Sequence as S
import Data.Maybe

--------------------------------------------------
-- DList

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($[]) . unDL
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (singleton n `append` xs)

--------------------------------------------------
-- Queue

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue xs ys) = Queue (x:xs) ys

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue xs []) = Just (head xs', Queue [] (tail xs'))
  where xs' = reverse xs
pop (Queue xs ys) = Just (head ys, Queue xs (tail ys))

fromList :: [a] -> Queue a
fromList xs = Queue [] (reverse xs)

constructQueue :: Int -> Queue Int
constructQueue i = go i (Queue [] [])
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (push n xs)

deconstructQueue :: Queue Int -> Int
deconstructQueue xs = go 0 xs
  where go n xs = case (pop xs) of
          Nothing -> n
          Just (_, xs') -> go (n+1) xs'

--------------------------------------------------

constructSeq :: Int -> S.Seq Int
constructSeq i = go i S.empty
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (n S.<| xs)

deconstructSeq :: S.Seq Int -> Int
deconstructSeq xs = go 1 xs
  where go n xs =
          let
            last' = S.length xs - 1
            xs' = S.deleteAt last' xs
          in
            if xs' == S.empty
            then n
            else go (n+1) xs'
            
--------------------------------------------------

main :: IO ()
main = defaultMain
  [ bench "concat list" $
    whnf schlemiel 123456
  , bench "concat dlist" $
    whnf constructDlist 123456
  , bench "construct sequence" $
    whnf constructSeq 123456
  , bench "construct queue" $
    whnf constructQueue 123456
  , bench "deconstruct sequence" $
    whnf deconstructSeq (S.fromList [1..123456])
  , bench "deconstruct queue" $
    whnf deconstructQueue (fromList [1..123456])
  ]
