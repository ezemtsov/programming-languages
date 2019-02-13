import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome $ cleanStr line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
  where isPalindrome str = case str of
          "" -> False
          _  -> str == (reverse $ cleanStr str)
        cleanStr =
            filter isLetter
          . map toLower

--------------------------------------------------

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Provide a name of a person:"
  name <- getLine
  putStrLn "Provide an age of a person:"
  age <- getLine
  let newSomeone =
        mkPerson name (read age :: Age)
  case newSomeone of
    Left err ->
      putStrLn $ "Oops, error:\n" ++ (show err)
    Right p ->
      putStrLn $ "Yay! Successfully got a person:\n"
      ++ (show p)
