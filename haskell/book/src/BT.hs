{-# LANGUAGE OverloadedStrings #-}

module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString
  as A
import Data.Attoparsec.ByteString
  (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a
      => Parser a
      -> String -> IO ()
trifP p i =
  print $ parseString p mempty i

parsecP :: Show a
        => Parsec String () a
        -> String -> IO ()
parsecP = parseTest        

attoP :: Show a
      => A.Parser a
      -> ByteString -> IO ()
attoP p i =
  print $ parseOnly p i

nobackParse :: (Monad f, CharParsing f)
            => f Char
nobackParse =
      (char '1' >> char '2')
  <|> char '3'

tryParse :: (Monad f, CharParsing f)
         => f Char
tryParse =
      try (char '1' >> char '2')
  <|> char '3'

tryAnnot :: (Monad f, CharParsing f)
         => f Char
tryAnnot =
      (try (char '1' >> char '2')
       <?> "Tried 12")
  <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  -- trifecta
  trifP nobackParse "12"
  trifP tryParse "12"
  -- parsec
  parsecP nobackParse "12"
  parsecP tryParse "12"
  -- atooparsec
  attoP nobackParse "12"
  attoP tryParse "12"
