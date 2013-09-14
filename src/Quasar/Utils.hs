module Quasar.Utils where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W (run)
import System.Exit
import System.IO

-- TODO: move to separate module
eitherDecodeBs :: FromJSON a => BS.ByteString -> Either String a
eitherDecodeBs = eitherDecode . bsToLbs

bsToLbs :: BS.ByteString -> LBS.ByteString
bsToLbs = LBS.fromChunks . BS.lines

lbsToBs :: LBS.ByteString -> BS.ByteString
lbsToBs = BS.concat . LBS.toChunks

warp :: Int -> W.Application -> IO ()
warp port app = W.run port app

forkWarp :: Int -> W.Application -> IO ThreadId
forkWarp port app = forkIO $ W.run port app

exitOnInput = do
  hSetBuffering stdin NoBuffering
  _ <- getChar
  exitSuccess