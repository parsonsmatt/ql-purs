module QuickLift.Model.User where

import BigPrelude

import qualified Data.String as Str
import Unsafe.Coerce

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

newtype User
  = User
  { name :: String
  , email :: String
  }

mkUser :: String -> String -> User
mkUser n e = User { name: n, email: e }

derive instance genericUser :: Generic User

instance showUser :: Show User where
  show = gShow

instance respondableUser :: Respondable User where
  responseType =
    JSONResponse
  fromResponse json =
    mkUser <$> readProp "name" json 
           <*> readProp "email" json

