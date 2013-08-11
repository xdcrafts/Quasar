module Quasar.Api.Http.Request where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Conduit.List (consume)
import Data.Conduit
import qualified Data.Text as T
import Data.Monoid
import Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI
import qualified Network.HTTP.Types.Header as H
import qualified Network.Wai as W
import Quasar.Api.Http.Header
import Quasar.Api.Http.Query

data Request a = Request 
  { requestMethod   :: StdMethod
  , requestHeaders  :: Headers
  , requestQuery    :: Query
  , requestPath     :: [String]
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

parseRawRequestBody :: W.Request -> IO String
parseRawRequestBody warpRequest = BS.unpack <$> mconcat <$> runResourceT (W.requestBody warpRequest $$ consume)

buildRequest :: W.Request -> String -> Maybe (Request String)
buildRequest warpRequest body = case parseMethod . W.requestMethod $ warpRequest of
  Left _          -> Nothing
  Right stdMethod -> Just Request
    { requestMethod   = stdMethod
    , requestHeaders  = fromStdHeaders $ W.requestHeaders warpRequest
    , requestQuery    = fromStdQuery $ W.queryString warpRequest
    , requestPath     = map T.unpack (W.pathInfo warpRequest)
    , requestBody     = body
    }