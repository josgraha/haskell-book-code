module Main (main) where

import           Control.Monad (forever)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

messageLength :: Int
messageLength = 1024

fingerPort :: Int
fingerPort = 79

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
    (s, _) <- accept sock
    printAndSendBack s
    sClose s
    where printAndSendBack conn = do
            message <- recv conn messageLength
            print message
            sendAll conn message

main :: IO ()
main = withSocketsDo $ do
    addrInfos <- getAddrInfo
        (Just (defaultHints { addrFlags = [ AI_PASSIVE ] }))
        Nothing (Just $ show fingerPort)
    let addrInfo = head addrInfos
    sock <- socket (addrFamily addrInfo) Stream defaultProtocol
    bindSocket sock (addrAddress addrInfo)
    listen sock 1
    logAndEcho sock
    sClose sock
