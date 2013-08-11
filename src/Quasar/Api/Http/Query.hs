module Quasar.Api.Http.Query where

import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types.URI as URI

type Query 	   = [QueryItem]
type QueryItem = (String, Maybe String)

fromStdQuery :: URI.Query -> Query
fromStdQuery = map (\(key, maybeValue) -> (BS.unpack key, fmap BS.unpack maybeValue))