{-# LANGUAGE OverloadedStrings #-}
module Quasar.Api.Routing where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.CaseInsensitive
import Data.Conduit (ResourceT)
import Data.Monoid
import Data.Text
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Network.Wai as W
import Quasar.Api.Http.Request
import Quasar.Api.Http.Response

data Route = Route StdMethod [Text]
  deriving (Eq, Show)

type Router = Route -> Request BS.ByteString -> Maybe (Response (Maybe BS.ByteString))

routedApplication :: Router -> W.Request -> ResourceT IO W.Response
routedApplication router warpRequest = do
  requestBody <- liftIO . parseRawRequestBody $ warpRequest
  let maybeRequest = buildRequest warpRequest requestBody
      qresponse    = case maybeRequest of
        Nothing      -> badRequestResponse
        Just request ->
          let route = Route (requestMethod request) (requestPath request)
              maybeResponse = router route request
          in case maybeResponse of
          	Nothing       -> badRequestResponse
          	Just response -> response
  return $ buildResponse qresponse