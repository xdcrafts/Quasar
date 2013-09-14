{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
module Quasar.Api.QuasarAction where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Types.Status
import Quasar.Api.Http.Request
import Quasar.Api.Http.Response
import Quasar.Utils

-- TODO: think about Write monad here
type ErrorMessage = String
type RequestTransformer a  = Request BS.ByteString -> Either String (Request a)
type ResponseTransformer b = Either ErrorMessage (Response (Maybe b)) -> Response (Maybe LBS.ByteString)
type QuasarActionType a b  = Request a -> Either ErrorMessage (Response (Maybe b))

data QuasarAction a b = QuasarAction {
  _requestTransformer    :: RequestTransformer a
  , _responseTransformer :: ResponseTransformer b
  , _quasarAction        :: QuasarActionType a b
}

$(makeLenses ''QuasarAction)

runAction :: Request BS.ByteString -> QuasarAction a b -> Response (Maybe LBS.ByteString)
runAction requestBodyString action = 
  let requestBody = action^.requestTransformer $ requestBodyString
      response = case requestBody of
        Left error -> Left error
        Right rqst -> action^.quasarAction $ rqst
  in action^.responseTransformer $ response

applyTypedJsonAction :: (?rqst :: Request BS.ByteString, FromJSON a, ToJSON b) => QuasarActionType a b -> Maybe (Response (Maybe LBS.ByteString))
applyTypedJsonAction action = Just $ runAction ?rqst $ typedJsonAction action

maybeEncodeToJsonEither :: ToJSON a => ResponseTransformer a
maybeEncodeToJsonEither either = case either of
  Right response -> response `mapResponseBody` (fmap encode)
  Left err       -> Response { _responseStatus  = status500
                             , _responseHeaders = [("Content-Type", "text/html")]
                             , _responseBody    = Just $ LBS.pack err
                             }

typedJsonAction :: (FromJSON a, ToJSON b) => QuasarActionType a b -> QuasarAction a b
typedJsonAction action = QuasarAction {
  _requestTransformer    = eitherMapRequestBody eitherDecodeBs
  , _responseTransformer = maybeEncodeToJsonEither
  , _quasarAction        = action
}