{-# LANGUAGE NoMonomorphismRestriction #-}

module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1' >> eof

-- read a single character '1', then die
one' = one >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2' >> eof

-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Show a => Parser a -> IO ()
testParse p =
  print $ parseString p mempty "123"


string' :: CharParsing m => String -> m String
string' [] = pure []
string' (x:xs) = (:) <$> (char x) <*> (string' xs)

pNL s =
  putStrLn ('\n' : s)

main = do
  --pNL "stop:"
  --testParse stop
  pNL "one:"
  testParse one
  --pNL "one':"
  --testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  --pNL "oneTwo':"
  --testParse oneTwo'
  pNL "stringOne:"
  testParse $ string' "1"
  pNL "stringOneTwo:"
  testParse $ string' "12"
  pNL "stringOneTwoThree:"
  testParse $ string' "123"

