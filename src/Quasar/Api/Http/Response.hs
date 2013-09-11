{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Quasar.Api.Http.Response where

import Control.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
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

badRequestResponse :: Response (Maybe BS.ByteString)
badRequestResponse = Response
  { _responseStatus  = status404
  , _responseHeaders = []
  , _responseBody    = Just "Bad Request"
  }

filterHeaders :: Response a -> (Header -> Bool) -> Response a
filterHeaders response f = responseHeaders .~ (filter f $ response^.responseHeaders) $ response

withHeaders :: Response a -> ResponseHeaders -> Response a
withHeaders response headers = responseHeaders .~ (headers ++ response^.responseHeaders) $ response

mapBody :: Response a -> (a -> b) -> Response b
mapBody response f = responseBody .~ (f $ response^.responseBody) $ response

buildResponse :: Response (Maybe BS.ByteString) -> W.Response
buildResponse response =
  W.responseLBS
    (response^.responseStatus)
    (response^.responseHeaders)
    body
  where body = case response^.responseBody of Nothing         -> LBS.empty
                                              Just bodyString -> bsToLbs bodyString