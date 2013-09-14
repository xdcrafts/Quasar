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

filterHeaders :: Request a -> (Header -> Bool) -> Request a
filterHeaders request f = requestHeaders .~ (filter f $ request^.requestHeaders) $ request

mapRequestBody :: (a -> b) -> Request a -> Request b
mapRequestBody f request = requestBody .~ (f $ request^.requestBody) $ request

eitherMapRequestBody :: (a -> Either String b) -> Request a -> Either String (Request b)
eitherMapRequestBody f request = case f $ request^.requestBody of
  Left error -> Left error
  Right body -> Right (requestBody .~ body $ request)

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