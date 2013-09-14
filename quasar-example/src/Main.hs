{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Quasar.Api.Http.Response
import Quasar.Api.Http.Request
import Quasar.Api.Routing
import Quasar.Api.Action as Q
import Quasar.Utils

data User = User {
  userId     :: Maybe String
  , login    :: String
  , password :: String
}

$(deriveJSON (drop 0) ''User)

data Session = Session {
  sessionId :: Maybe String
}

$(deriveJSON (drop 0) ''Session)

userAction :: Q.ActionType User User
userAction rqst = Right ok { _responseBody = Just $ rqst^.requestBody }

sessionAction :: Q.ActionType Session Session
sessionAction rqst = Right ok { _responseBody = Just $ rqst^.requestBody }

actionRouter :: Route -> Request BS.ByteString -> Maybe (Response (Maybe LBS.ByteString))
actionRouter route request = let ?rqst = request in	case route of
  Route PUT  _ -> applyTypedJsonAction userAction
  Route POST ["v1", "session"] -> applyTypedJsonAction sessionAction
  Route POST ["v1", "tst"] -> Just Response { _responseStatus  = status200
                                            , _responseHeaders = [("Content-Type", "text/html")]
                                            , _responseBody    = Just $ bsToLbs $ request^.requestBody
                                            }
  otherwise    -> Nothing

main :: IO ()
main = do
  putStrLn "http://localhost:8080"
  forkWarp 8080 (routedApplication actionRouter)
  putStrLn "[ Press Enter for shutdown ]"
  exitOnInput