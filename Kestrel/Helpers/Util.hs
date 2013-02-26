module Kestrel.Helpers.Util 
       ( encodeUrl
       , decodeUrl
       ) where

import Codec.Binary.UTF8.String (encodeString, decodeString)
import qualified Data.Text as T
import Network.HTTP.Base (urlEncode, urlDecode)

encodeUrl :: T.Text -> T.Text
encodeUrl = T.pack . urlEncode . encodeString . T.unpack

decodeUrl :: T.Text -> T.Text
decodeUrl = T.pack . decodeString . urlDecode . T.unpack
