module Server
    (server) 
    where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Control.Monad
import Network.Socket
import Common (talk, receiveMsg)

server :: [String] -> IO ()
server [port] = withSocketsDo $ do
    putStrLn $ "Running socket server on port " ++ port
    addr <- resolve port
    bracket (open addr) close loop
    putStrLn "Stopped socket server"
server [] = do
    putStrLn "No port informed"

resolve port = do
    let hints = defaultHints {
        addrFlags = [AI_PASSIVE], 
        addrSocketType = Stream
    }
    addr: _ <- getAddrInfo (Just hints) Nothing (Just port)
    return addr

open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    withFdSocket sock (\fd -> do 
        setSocketOption sock ReuseAddr 1
        setCloseOnExecIfNeeded fd)
    bind sock (addrAddress addr)
    listen sock 10
    return sock

loop sock = forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from " ++ show peer
    void $ forkFinally (receiveMsg conn "Client said: ") (\_ -> close conn)
    talk conn
