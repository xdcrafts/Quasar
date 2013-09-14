{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Quasar.Api.Http.Response where

import Control.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.CaseInsensitive
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Network.Wai as W
import Quasar.Utils

data Response a = Response
  { _responseStatus  :: Status
  , _responseHeaders :: ResponseHeaders
  , _responseBody    :: a
  }
  deriving (Eq, Show)

$(makeLenses ''Response)

badRequestResponse :: Response (Maybe LBS.ByteString)
badRequestResponse = Response
  { _responseStatus  = status404
  , _responseHeaders = []
  , _responseBody    = Just "Bad request"
  }

filterHeaders :: Response a -> (Header -> Bool) -> Response a
filterHeaders response f = responseHeaders .~ (filter f $ response^.responseHeaders) $ response

withHeaders :: Response a -> ResponseHeaders -> Response a
withHeaders response headers = responseHeaders .~ (headers ++ response^.responseHeaders) $ response

mapResponseBody :: Response a -> (a -> b) -> Response b
mapResponseBody response f = responseBody .~ (f $ response^.responseBody) $ response

buildResponse :: Response (Maybe LBS.ByteString) -> W.Response
buildResponse response =
  W.responseLBS
    (response^.responseStatus)
    (response^.responseHeaders)
    body
  where body = case response^.responseBody of Nothing         -> LBS.empty
                                              Just bodyString -> bodyString