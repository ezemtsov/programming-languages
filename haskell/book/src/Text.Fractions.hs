{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  print $ parseFraction' badFraction
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork

-- Ex. Unit of Success
-- Return result of parse on eof

parseInteger :: Parser Integer
parseInteger = do
  value <- integer
  endOfFile <- eof
  return value

testParseInteger :: IO ()
testParseInteger = do
  let parseInteger' =
        parseString parseInteger mempty
  print $ parseInteger' "123"
  print $ parseInteger' "123abc"

--------------------------------------------------
-- Exercise: Try Try
-- Make a parser, using the existing fraction
-- parser plus a new decimal parser, that can
-- parse either decimals or fractions. You'll
-- want to use <|> from Alternative to combine
-- the...alternative parsers.

countDigits :: (Show a, Num a)
            => a -> Int
countDigits = length . show

parseDecimal :: Parser Double
parseDecimal = do
  x <- decimal
  char '.'
  y <- decimal
  let x' = fromInteger x
      y' = fromInteger y
  return (x' + y' / 10 ^ (length . show $ y))

data FracOrDec = Frac Rational | Dub Double
  deriving (Eq, Show)

parseFracOrDec :: Parser FracOrDec
parseFracOrDec = do
  num <- decimal
  v <-     (Left <$> char '/')
       <|> (Right <$> char '.')
  denum <- decimal
  return $ case v of
    Left _ -> Frac (num % denum)
    Right _ -> Dub (fromInteger num + fromInteger denum
                          / 10 ^ (length . show $ denum))

parseFracOrDec' :: Parser (Either Rational Double)
parseFracOrDec' = try (Left <$> virtuousFraction)
                  <|> (Right <$> double)
