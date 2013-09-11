{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Quasar.Api.Http.Request where

import Control.Applicative
import Control.Lens
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
  { _requestMethod   :: StdMethod
  , _requestHeaders  :: RequestHeaders
  , _requestQuery    :: Query
  , _requestPath     :: [T.Text]
  , _requestBody     :: a
  }
  deriving (Eq, Show)

$(makeLenses ''Request)

filterHeaders :: (Header -> Bool) -> Request a -> Request a
filterHeaders f r = requestHeaders .~ (filter f $ r^.requestHeaders) $ r

mapBody ::  (a -> b) -> Request a -> Request b
mapBody f r = requestBody .~ (f $ r^.requestBody) $ r

parseRawRequestBody :: W.Request -> IO BS.ByteString
parseRawRequestBody warpRequest = mconcat <$> runResourceT (W.requestBody warpRequest $$ consume)

buildRequest :: W.Request -> BS.ByteString -> Maybe (Request BS.ByteString)
buildRequest warpRequest body = case parseMethod . W.requestMethod $ warpRequest of
  Left _          -> Nothing
  Right stdMethod -> Just Request
    { _requestMethod   = stdMethod
    , _requestHeaders  = W.requestHeaders warpRequest
    , _requestQuery    = W.queryString warpRequest
    , _requestPath     = W.pathInfo warpRequest
    , _requestBody     = body
    }