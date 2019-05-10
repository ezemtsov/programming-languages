{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer
  Major Minor Patch
  Release Metadata
  deriving (Show, Eq)

preReleaseTest :: [String]
preReleaseTest = [ "1.0.0"
                 , "1.0.0-alpha"
                 , "1.0.0-alpha.1"
                 , "1.0.0-0.3.7"
                 , "1.0.0-x.7.z.92" ]

buildMDTest :: [String]
buildMDTest = [ "1.0.0-alpha+001"
              , "1.0.0+20130313144700"
              , "1.0.0-beta+exp.sha.5114f85"]

allowed = ['0'..'9']
       ++ ['A'..'Z']
       ++ ['a'..'z']
       ++ ['-','.']

parseNos :: Parser NumberOrString
parseNos =
  try (NOSI <$> integer <* notFollowedBy letter)
  <|> (NOSS <$> some letter <* notFollowedBy integer)

parseRelease :: Parser NumberOrString
parseRelease = skipMany (oneOf ".") >> parseNos

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  skipSome (oneOf ".")
  minor <- integer
  skipSome (oneOf ".")
  patch <- integer
  release <-
    try (char '-' *> some parseRelease)
    <|> (mempty)
  metadata <-
    try (char '+' *> some parseRelease)
    <|> (mempty)
  return $ SemVer
    major minor patch
    release metadata

instance Ord NumberOrString where
  compare (NOSI _) (NOSS _) = LT
  compare (NOSS _) (NOSI _) = GT
  compare (NOSS a) (NOSS b) = compare a b
  compare (NOSI a) (NOSI b) = compare a b

instance Ord SemVer where
  compare (SemVer a1 b1 c1 r1 e1) (SemVer a2 b2 c2 r2 e2) =
       compare a1 a2
    <> compare b1 b2
    <> compare c1 c2
    <> compare r1 r2

main = do
  let p f i = parseString f mempty i
  sequence $ print
    <$> p parseSemVer
    <$> (preReleaseTest ++ buildMDTest)
