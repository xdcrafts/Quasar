{-# LANGUAGE OverloadedStrings #-}
module Quasar.Api.Http.Response where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Network.Wai as W
import Quasar.Utils

data Response a = Response
  { responseStatus  :: Status
  , responseHeaders :: ResponseHeaders
  , responseBody    :: a
  }
  deriving (Eq, Show)

badRequestResponse :: Response (Maybe BS.ByteString)
badRequestResponse = Response
  { responseStatus  = status404
  , responseHeaders = []
  , responseBody    = Just "Bad Request"
  }

filterHeaders :: Response a -> (Header -> Bool) -> Response a
filterHeaders response f = Response
  { responseStatus  = responseStatus response
  , responseHeaders = filter f (responseHeaders response)
  , responseBody    = responseBody response
  }

withHeaders :: Response a -> ResponseHeaders -> Response a
withHeaders response headers = Response
  { responseStatus  = responseStatus response
  , responseHeaders = (responseHeaders response) ++ headers
  , responseBody    = responseBody response
  }

mapBody :: Response a -> (a -> b) -> Response b
mapBody response f = Response
  { responseStatus  = responseStatus response
  , responseHeaders = responseHeaders response
  , responseBody    = f $ responseBody response
  }

buildResponse :: Response (Maybe BS.ByteString) -> W.Response
buildResponse response =
  W.responseLBS
    (responseStatus response)
    (responseHeaders response)
    body
  where body = case responseBody response of Nothing         -> LBS.empty
                                             Just bodyString -> bsToLbs bodyString