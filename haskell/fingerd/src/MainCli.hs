{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.Environment (getArgs)
import Text.RawString.QQ
import Data.List (intersperse)

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
  hiding (intersperse)
import Network.Socket
  hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

help :: String
help = [r|
--------------------------------------------------
  Usage:
    fingerC <ip> <command>
  Command list:
    list                      - get user list
    show <u>                  - show username
    delete <u>                - remove user
    alter <u> <s> <h> <r> <p> - add or update user
--------------------------------------------------
|]

isIpAddress :: ByteString -> Bool
isIpAddress =
    go (4 :: Int)
  where
    go 0 bs = BS.null bs
    go rest bs =
        case C.readInt x of
            Just (i, x') | BS.null x' && i >= 0 && i < 256 -> go (rest - 1) y
            _ -> False
      where
        (x, y') = BS.break (== 46) bs -- period
        y = BS.drop 1 y'
  
main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  case args of
    ip:_ -> do
      if isIpAddress (C.pack ip)
        then do
          addr <- resolve ip "78"
          let cmd = C.pack $ (concat $ intersperse "|" (tail args)) ++ "|"
          E.bracket (open addr) close (talk cmd)
        else
          putStrLn help
    _ -> do
      putStrLn help
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talk cmd sock = do
        sendAll sock cmd
        msg <- recv sock 1024
        C.putStrLn msg
