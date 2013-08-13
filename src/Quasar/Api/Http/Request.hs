{-# LANGUAGE OverloadedStrings #-}
module Quasar.Api.Http.Request where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Conduit.List (consume)
import Data.Conduit
import qualified Data.Text as T
import Data.Monoid
import Network.HTTP.Types.Method
import Network.HTTP.Types.URI
import Network.HTTP.Types.Header
import qualified Network.Wai as W

data Request a = Request 
  { requestMethod   :: StdMethod
  , requestHeaders  :: RequestHeaders
  , requestQuery    :: Query
  , requestPath     :: [T.Text]
  , requestBody     :: a
  }
  deriving (Eq, Show)

filterHeaders :: (Header -> Bool) -> Request a -> Request a
filterHeaders f r = Request
  { requestMethod   = requestMethod r
  , requestHeaders  = filter f (requestHeaders r)
  , requestQuery    = requestQuery r
  , requestPath     = requestPath r
  , requestBody     = requestBody r
  }

mapBody :: Request a -> (a -> b) -> Request b
mapBody r f = Request
  { requestMethod   = requestMethod r
  , requestHeaders  = requestHeaders r
  , requestQuery    = requestQuery r
  , requestPath     = requestPath r
  , requestBody     = f . requestBody $ r
  }

parseRawRequestBody :: W.Request -> IO BS.ByteString
parseRawRequestBody warpRequest = mconcat <$> runResourceT (W.requestBody warpRequest $$ consume)

buildRequest :: W.Request -> BS.ByteString -> Maybe (Request BS.ByteString)
buildRequest warpRequest body = case parseMethod . W.requestMethod $ warpRequest of
  Left _          -> Nothing
  Right stdMethod -> Just Request
    { requestMethod   = stdMethod
    , requestHeaders  = W.requestHeaders warpRequest
    , requestQuery    = W.queryString warpRequest
    , requestPath     = W.pathInfo warpRequest
    , requestBody     = body
    }