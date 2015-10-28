module QuickLift.Model where

import BigPrelude

import qualified Data.String as Str
import Unsafe.Coerce

import Data.Foreign.Class
import Data.Generic
import Data.Date hiding (fromString)
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

newtype Session
  = Session
  { date :: Date
  , text :: String
  }


instance encodeSession :: EncodeJson Session where
  encodeJson (Session s) =
       "text" := s.text
    ~> "date" := renderDate s.date
    ~> "userId" := 1
    ~> jsonEmptyObject


instance requestableSession :: Requestable Session where
  toRequest s =
    let str = printJson (encodeJson s) :: String
     in toRequest str


mkSession :: Date -> String -> Session
mkSession d t = Session { date: d, text: t }

getSessionDate :: Session -> Date
getSessionDate (Session d) = d.date

getSessionText :: Session -> String
getSessionText (Session s) = s.text


renderDate :: Date -> String
renderDate date = y ++ "-" ++ m ++ "-" ++ d
  where
    y = (ypad <<< show) case year date of Year n -> n
    m = pad (1 + (fromEnum $ month date))
    d = pad case dayOfMonth date of DayOfMonth day -> day
    pad n = let str = show n
             in case Str.length str of
                   1 ->  "0" ++ str
                   _ -> str
    ypad str =
      case Str.length str of
           0 -> "0000"
           1 -> "000" ++ str
           2 -> "00" ++ str
           3 -> "0" ++ str
           _ -> str

