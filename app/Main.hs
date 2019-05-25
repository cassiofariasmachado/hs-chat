module Main (main) where

import Server
import Client
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("server", server),
              ("client", client) ]  

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch  
    action args