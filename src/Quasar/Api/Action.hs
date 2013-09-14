{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
module Quasar.Api.Action where

import Control.Lens hiding (Action)
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
type ActionType a b        = Request a -> Either ErrorMessage (Response (Maybe b))

data Action a b = Action {
  _requestTransformer    :: RequestTransformer a
  , _responseTransformer :: ResponseTransformer b
  , _quasarAction        :: ActionType a b
}

$(makeLenses ''Action)

withoutRequestTransformation :: RequestTransformer BS.ByteString
withoutRequestTransformation = Right

maybeEncodeToJsonEither :: ToJSON a => ResponseTransformer a
maybeEncodeToJsonEither either = case either of
  Right response -> withHeaders (response `mapResponseBody` (fmap encode)) [("Content-Type", "application/json")]
  Left err       -> Response { _responseStatus  = status500
                             , _responseHeaders = [("Content-Type", "text/html")]
                             , _responseBody    = Just $ LBS.pack err
                             }

runAction :: Request BS.ByteString -> Action a b -> Response (Maybe LBS.ByteString)
runAction requestBodyString action = 
  let requestBody = action^.requestTransformer $ requestBodyString
      response = case requestBody of
        Left error -> Left error
        Right rqst -> action^.quasarAction $ rqst
  in action^.responseTransformer $ response

applyHttpToJsonAction :: (?rqst :: Request BS.ByteString, ToJSON b) => ActionType BS.ByteString b -> Maybe (Response (Maybe LBS.ByteString))
applyHttpToJsonAction action = Just $ runAction ?rqst $ httpToJsonAction action

applyTypedJsonAction :: (?rqst :: Request BS.ByteString, FromJSON a, ToJSON b) => ActionType a b -> Maybe (Response (Maybe LBS.ByteString))
applyTypedJsonAction action = Just $ runAction ?rqst $ typedJsonAction action

httpToJsonAction :: ToJSON b => ActionType BS.ByteString b -> Action BS.ByteString b
httpToJsonAction action = Action {
  _requestTransformer    = withoutRequestTransformation
  , _responseTransformer = maybeEncodeToJsonEither
  , _quasarAction        = action
}

typedJsonAction :: (FromJSON a, ToJSON b) => ActionType a b -> Action a b
typedJsonAction action = Action {
  _requestTransformer    = eitherMapRequestBody eitherDecodeBs
  , _responseTransformer = maybeEncodeToJsonEither
  , _quasarAction        = action
}