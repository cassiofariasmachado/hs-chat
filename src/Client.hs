module Client
    (client) 
    where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Network.Socket
import Control.Monad
import Common (talk, receiveMsg)

client :: [String] -> IO ()
client [port] = withSocketsDo $ do
    putStrLn $ "Connection to socket server on port " ++ port
    addr <- resolve "127.0.0.1" port
    bracket (open addr) close waitMsgs
server [] = do
    putStrLn "No port informed"

resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    return addr

open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return sock

waitMsgs sock = do
    void $ forkFinally (receiveMsg sock "Server said: ") (\_ -> close sock)
    talk sock