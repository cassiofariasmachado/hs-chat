module Common
    (talk, receiveMsg) 
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad

-- obtém as mensagens escritas no terminal (stdin) e envia ao socket 
talk sock = forever $ do
    msg <- getLine

    case msg of
        "fim" -> error "Client closes connection"
        _ -> do sendAll sock $ C.pack msg
                putStrLn $ "You said: " ++ msg

-- recebe as mensagens do socket e escreve no terminal (stdout)
receiveMsg sock prefix = do
    msg <- recv sock 1024
    let formatedMsg = prefix ++ (C.unpack msg)

    unless (B.null msg) $ do
        putStrLn formatedMsg

    receiveMsg sock prefix