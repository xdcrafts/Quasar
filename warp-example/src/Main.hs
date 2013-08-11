{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.IO
import System.Exit
import Control.Concurrent

app :: Application
app _ = return $ responseLBS status200 [("Content-Type", "text/html")] "<h1>Hello from Warp!</h1>"

warp :: IO ()
warp = run 8080 app

main :: IO ()
main = do
  putStrLn "http://localhost:8080"
  forkIO warp
  putStrLn "[ Press Enter for shutdown ]"
  exitOnInput

exitOnInput = do
  hSetBuffering stdin NoBuffering
  _ <- getChar
  exitSuccess