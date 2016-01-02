module QuickLift.Model.User where

import BigPrelude

import Data.String as Str
import Unsafe.Coerce

import Optic.Lens
import Optic.Core

import Data.Foreign hiding (isNull, isArray)
import Data.Foreign.Class
import Data.Generic
import Data.Date hiding (fromString)
import Data.Date as Date
import Data.Date.UTC

import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Printer

import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response

newtype User
  = User
  { name :: String
  , email :: String
  }

_User :: LensP User { name :: String, email :: String }
_User f (User o) = User <$> f o

name :: forall b a r. Lens { name :: a | r } { name :: b | r } a b
name f o = o { name = _ } <$> f o.name

email :: forall b a r. Lens { email :: a | r } { email :: b | r } a b
email f o = o { email = _ } <$> f o.email

emptyUser :: User
emptyUser = User { name: "", email: "" }

mkUser :: String -> String -> User
mkUser n e = User { name: n, email: e }

derive instance genericUser :: Generic User

instance showUser :: Show User where
  show = gShow

instance respondableUser :: Respondable User where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
    mkUser <$> readProp "name" json
           <*> readProp "email" json

instance requestableUser :: Requestable User where
  toRequest s =
    let str = printJson (encodeJson s) :: String
     in toRequest str

instance isForeignUser :: IsForeign User where
  read f = mkUser
    <$> readProp "name" f
    <*> readProp "email" f

instance encodeUser :: EncodeJson User where
  encodeJson (User u) =
       "name"  := u.name
    ~> "email" := u.email
    ~> jsonEmptyObject

instance decodeUser :: DecodeJson User where
  decodeJson = gDecodeJson
