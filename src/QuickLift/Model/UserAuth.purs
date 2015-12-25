module QuickLift.Model.UserAuth where

import BigPrelude

import qualified Data.String as Str

import Optic.Lens
import Optic.Core

import Data.Foreign hiding (isNull, isArray)
import Data.Foreign.Class
import Data.Generic
import Data.Date hiding (fromString)
import qualified Data.Date as Date
import Data.Date.UTC

import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Printer

import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response

newtype UserAuth
  = UserAuth
  { email :: String
  , password :: String
  , confirmation :: String
  }

_UserAuth :: LensP UserAuth { email :: String, password :: String, confirmation :: String }
_UserAuth f (UserAuth o) = UserAuth <$> f o

emptyAuth :: UserAuth
emptyAuth = UserAuth
  { email: ""
  , password: ""
  , confirmation: ""
  }

mkUserAuth :: String -> String -> String -> UserAuth
mkUserAuth e p pc =
  UserAuth { email: e, password: p, confirmation: pc }

derive instance genericUserAuth :: Generic UserAuth

instance showUserAuth :: Show UserAuth where show = gShow

instance eqUserAuth :: Eq UserAuth where eq = gEq

instance respondableUserAuth :: Respondable UserAuth where
  responseType =
    JSONResponse
  fromResponse json = mkUserAuth
    <$> readProp "email" json
    <*> readProp "password" json
    <*> readProp "confirmation" json

instance requestableUserAuth :: Requestable UserAuth where
  toRequest s =
    let str = printJson (encodeJson s) :: String
     in toRequest str

instance encodeUserAuth :: EncodeJson UserAuth where
  encodeJson (UserAuth u) = 
       "email" := u.email
    ~> "password" := u.password
    ~> "confirmation" := u.confirmation
    ~> jsonEmptyObject

instance decodeUserAuth :: DecodeJson UserAuth where
  decodeJson = gDecodeJson

