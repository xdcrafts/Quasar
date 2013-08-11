module Quasar.Api.Http.Header where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types.Header as H

type Headers     = [Header]
type Header      = (HeaderName, HeaderValue)
type HeaderName  = String
type HeaderValue = String

toStdHeaders :: Headers -> [H.Header]
toStdHeaders = map (\(name, value) -> (CI.mk . BS.pack $ name, BS.pack value))

fromStdHeaders :: [H.Header] -> Headers
fromStdHeaders = map (\(headerName, headerValue) -> (BS.unpack . CI.original $ headerName, BS.unpack headerValue))