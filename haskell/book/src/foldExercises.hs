-- foldExercises.hs
module FoldExercises where

import Data.List

stops = "pbtdkg"
vowels = "aeiou"

ex1a = [x:y:z:[] | x <- stops,
                   y <- vowels,
                   z <- stops]

ex1b = [(x:xs) | (x:xs) <- ex1a, x == 'p']

nouns = ["Daffodil",
         "Formal",
         "Grassland",
         "Knife",
         "Sleuth"]
verbs = ["pretend",
         "fax",
         "wriggle",
         "stare",
         "move"]

ex1c = [concat(intersperse " " (x:y:z:[]))
       | x <- nouns,
         y <- verbs,
         z <- nouns]

-- this one takes string as parameter and
-- calculates average length of the word
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritFunc' x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))
