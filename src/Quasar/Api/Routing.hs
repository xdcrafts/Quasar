module Quasar.Api.Routing where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.CaseInsensitive
import Data.Conduit (ResourceT)
import Data.Monoid
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Network.Wai as W
import Quasar.Api.Http.Request
import Quasar.Api.Http.Response

data Route = Route StdMethod [String]
  deriving (Eq, Show)

type Router = Route -> Request String -> Maybe (Response (Maybe String))

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