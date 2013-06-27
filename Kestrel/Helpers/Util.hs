module Kestrel.Helpers.Util 
       ( encodeUrl
       , decodeUrl
       , ToText(..)
       ) where

import Prelude
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.Int (Int64)
import Network.HTTP.Base (urlEncode, urlDecode)

encodeUrl :: T.Text -> T.Text
encodeUrl = T.pack . urlEncode . encodeString . T.unpack

decodeUrl :: T.Text -> T.Text
decodeUrl = T.pack . decodeString . urlDecode . T.unpack

class ToText a where
  toText :: a -> Text

instance ToText String where
  toText = T.pack
instance ToText Text where
  toText = id
instance ToText Int where
  toText = T.pack . show
instance ToText Double where
  toText = T.pack . show
instance ToText Integer where
  toText = T.pack . show
instance ToText Day where
  toText = T.pack . show
instance ToText TimeOfDay where
  toText = T.pack . show
instance ToText UTCTime where
  toText = T.pack . show
instance ToText Int64 where
  toText = T.pack . show

