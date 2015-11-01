module QuickLift.Model.Registration where

import BigPrelude

import qualified Data.String as Str
import Unsafe.Coerce

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

newtype UserReg
  = UserReg
  { email :: String
  , password :: String
  , passwordConfirmation :: String
  }

_UserReg :: LensP UserReg { email :: String, password :: String, passwordConfirmation :: String }
_UserReg f (UserReg o) = UserReg <$> f o

password :: forall a b r. Lens { password :: a | r } { password :: b | r } a b
password f o = o { password = _ } <$> f o.password

passwordConfirmation :: forall a b r. Lens { passwordConfirmation :: a | r } { passwordConfirmation :: b | r } a b
passwordConfirmation f o = o { passwordConfirmation = _ } <$> f o.passwordConfirmation

emptyReg :: UserReg
emptyReg = UserReg
  { email: ""
  , password: ""
  , passwordConfirmation: ""
  }

mkRegistration :: String -> String -> String -> UserReg
mkRegistration e p pc =
  UserReg { email: e, password: p, passwordConfirmation: pc }

derive instance genericUserReg :: Generic UserReg

instance showUserReg :: Show UserReg where show = gShow

instance eqUserReg :: Eq UserReg where eq = gEq

instance respondableUserReg :: Respondable UserReg where
  responseType =
    JSONResponse
  fromResponse json = mkRegistration
    <$> readProp "email" json
    <*> readProp "password" json
    <*> readProp "passwordConfirmation" json

instance requestableUserReg :: Requestable UserReg where
  toRequest s =
    let str = printJson (encodeJson s) :: String
     in toRequest str

instance encodeUserReg :: EncodeJson UserReg where
  encodeJson (UserReg u) = 
       "email" := u.email
    ~> "password" := u.password
    ~> "passwordconfirmation" := u.passwordConfirmation
    ~> jsonEmptyObject

instance decodeUserReg :: DecodeJson UserReg where
  decodeJson = gDecodeJson


