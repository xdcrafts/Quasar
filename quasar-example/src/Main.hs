{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Quasar.Api.Http.Header
import Quasar.Api.Http.Response
import Quasar.Api.Http.Request
import Quasar.Api.Routing
import Quasar.Utils

router :: Route -> Request String -> Maybe (Response (Maybe String))
router route request = case route of
  Route POST _ -> Just Response { responseStatus  = status200
                                , responseHeaders = [("Content-Type", "text/html")]
                                , responseBody    = Just $ requestBody request
                                }
  otherwise    -> Nothing

main :: IO ()
main = do
  putStrLn "http://localhost:8080"
  forkWarp 8080 (routedApplication router)
  putStrLn "[ Press Enter for shutdown ]"
  exitOnInput