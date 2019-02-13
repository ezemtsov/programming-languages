-- curry-exercise.hs
module CurryExercise where

str1 :: String
str1 = "Curry is awesome"

str2 :: String
str2 = "!"

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: String -> Int -> Char
letterIndex a b = a !! b

rvrs :: String
rvrs = (drop 9 x) ++ (take 4 (drop 5 x)) ++ (take 5 x)
  where x = str1

main :: IO ()
main = do
  putStrLn returnA
  putStrLn returnB
  putStrLn returnC
  putChar (thirdLetter returnA); putStrLn "";
  putStrLn [(letterIndex returnA 0)]
  putStrLn rvrs
  where returnA :: String
        returnA = str1 ++ str2
        returnB :: String
        returnB = [returnA !! 4]
        returnC :: String
        returnC = drop 9 returnA
  

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- appedCatty "woohoo!" "woops mrow woohoo!"
-- frappe "1" "1 mrow haha!"
-- appedCatty (frappe "blue") "woops mrow blue mrow hana"
-- cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) "pink mrow haha mrow green mrow woops mrow blue"
-- cattyConny (flippy "Pugs" "are") "awesome" "are mrow Pugs mrow awesome"
