module Client
    (client) 
    where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Network.Socket
import Control.Monad
import Common (talk, receiveMsg)

-- conecta-se via socket com o servidor local na porta informada 
client :: [String] -> IO ()
client [port] = withSocketsDo $ do
    putStrLn $ "Connection to socket server on port " ++ port
    addr <- resolve "127.0.0.1" port
    bracket (open addr) close waitMsgs
client [] = do
    putStrLn "No port informed"

-- obtém o endereço baseado no host e porta informado
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    return addr

-- conecta-se com o endereço informado e retorna um Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return sock

-- permite a troca de mensagens com o servidor
waitMsgs sock = do
    void $ forkFinally (receiveMsg sock "Server said: ") (\_ -> close sock)
    talk sock