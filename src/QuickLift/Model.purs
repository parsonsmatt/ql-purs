module QuickLift.Model where

import BigPrelude

import Data.Foreign.Class
import Data.Generic
import Data.Date
import Data.Date.UTC

import Network.HTTP.Affjax.Response

import Form

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

-- userForm :: Form User
-- userForm = mkUser <$> areq textField Nothing <*> areq textField Nothing
--   where
--     textField :: Form String
--     textField = Undefined

newtype Session
  = Session
  { date :: Date
  , text :: String
  }

mkSession :: Date -> String -> Session
mkSession d t = Session { date: d, text: t }
