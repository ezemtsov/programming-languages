{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
      -- that's one click!
      -- two..two clicks!
      -- three BEAUTIFUL clicks! ah ah ahhhh
      counts :: IORef (M.Map Text Integer)
    , prefix :: Text
    }

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Hadler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = case (m M.!? k) of
  Just i -> (,) (M.adjust (+1) k m) (i+1)
  Nothing -> (,) (M.insert k 1 m) 1

updCount :: (Config -> Text)
         -> Hadler Integer
updCount key' = lift . ReaderT $
  \conf -> do
  m <- readIORef (counts conf)
  let (m', i) = bumpBoomp (key' conf) m
  modifyIORef (counts conf) (const m')
  return i

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    let key' = mappend prefix (const unprefixed)
    newInteger <- updCount key'
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app
