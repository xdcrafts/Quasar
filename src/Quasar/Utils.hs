module Quasar.Utils where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W (run)
import System.Exit
import System.IO

warp :: Int -> W.Application -> IO ()
warp port app = W.run port app

forkWarp :: Int -> W.Application -> IO ThreadId
forkWarp port app = forkIO $ W.run port app

exitOnInput = do
  hSetBuffering stdin NoBuffering
  _ <- getChar
  exitSuccess