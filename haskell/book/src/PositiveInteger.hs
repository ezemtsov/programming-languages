{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- optional (char '-')
  case sign of
    Just _ -> negate <$> base10Integer
    Nothing -> base10Integer
