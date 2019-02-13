-- poemlines.hs

module PoemLines where
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand of eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen
            ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines s = go s []
  where go str result
         | str == "" = result
         | otherwise = go
                        (dropWhile (=='\n') (dropWhile (/='\n') str))
                        (result ++ [(takeWhile (/='\n') str)])


myLines':: String -> Char -> [String]
myLines' s sc = go s sc []
  where go str sc result
         | str == "" = result
         | otherwise = go
                        (dropWhile (== sc) (dropWhile (/= sc) str))
                        sc
                        (result ++ [(takeWhile (/= sc) str)])

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand of eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines' sentences '\n'
          == shouldEqual)
