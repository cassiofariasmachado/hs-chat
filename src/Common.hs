module Common
    (talk, receiveMsg) 
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad

talk sock = forever $ do
    msg <- getLine
    sendAll sock $ C.pack msg
    putStrLn $ "You said: " ++ msg

receiveMsg sock prefix = do
    msg <- recv sock 1024
    let formatedMsg = prefix ++ (C.unpack msg)

    unless (B.null msg) $ do
        putStrLn formatedMsg

    receiveMsg sock prefix