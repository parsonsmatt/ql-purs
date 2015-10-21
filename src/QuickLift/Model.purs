module QuickLift.Model where

import BigPrelude

import Data.Foreign.Class
import Data.Generic

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
