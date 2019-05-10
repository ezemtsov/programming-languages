import Text.Trifecta
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
  Exchange LineNumber
  deriving (Eq, Show)

separators = "+()- "

mergeInt :: [Integer] -> Integer
mergeInt = read . concat . fmap show

parseNumbers :: Parser Integer
parseNumbers = fmap mergeInt
  $ some $ do
    skipMany (oneOf separators)
    integer

parsePhone :: Parser PhoneNumber
parsePhone = do
  numbers <- fromInteger <$> parseNumbers
  let (p1p2, p3) = divMod numbers (10^4)
  let (pre, p2) = divMod p1p2 (10^3)
  let p1 = mod pre (10^3)
  return $ PhoneNumber p1 p2 p3

