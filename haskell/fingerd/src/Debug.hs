module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
  (recv, sendAll)

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  (soc, _) <- accept sock
  printAndKickback soc
  close soc
  where printAndKickback conn = do
          msg <- recv conn 1024
          print msg
          sendAll conn msg

type Port = Int

startServer :: Port -> IO ()
startServer port = withSocketsDo $ do
  addrinfos <- getAddrInfo
               (Just (defaultHints
                     {addrFlags =
                         [AI_PASSIVE]}))
               Nothing (Just $ show port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
          Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  putStrLn ("Server succesfully started on Port: "
            ++ (show port))
  logAndEcho sock
  close sock

main :: IO ()
main = forkIO (startServer 78)
       >> startServer 79
