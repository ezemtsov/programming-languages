module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ace", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) =>
               f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]]
             -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted ::
    (Functor f2, Functor f1, Functor f)
  => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted =
  (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]]
             -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "lms contents:\t"
  print lms
  
  putStr "replaceWithP' lms:\t"
  print (replaceWithP' lms)

  putStr "liftedReplace lms:\t"
  print (liftedReplace lms)

  putStr "twiceLifted lms:\t"
  print (twiceLifted lms)
  
  putStr "twiceLifted' lms:\t"
  print (twiceLifted' lms)

  putStr "thriceLifted lms:\t"
  print (thriceLifted lms)

  putStr "thriceLifted' lms:\t"
  print (thriceLifted' lms)
