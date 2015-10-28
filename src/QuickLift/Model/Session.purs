module QuickLift.Model.Session where

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

data Session
  = Session
  { date :: Date
  , text :: String
  , id :: Int
  , userId :: Int
  }

instance encodeSession :: EncodeJson Session where
  encodeJson (Session s) =
       "text" := s.text
    ~> "date" := dateIso8601 s.date
    ~> "userId" := 1
    ~> "id" := s.id
    ~> jsonEmptyObject


instance requestableSession :: Requestable Session where
  toRequest s =
    let str = printJson (encodeJson s) :: String
     in toRequest str

newtype ArrSession = ArrSession (Array Session)

unArrSession (ArrSession a) = a

instance isForeignArrSession :: IsForeign ArrSession where
  read f = ArrSession <$> read f

instance respondableArrSession :: Respondable ArrSession where
  responseType = JSONResponse
  fromResponse = read

instance sessionIsForeign :: IsForeign Session where
  read = readSession
    
readSession :: Foreign -> F Session
readSession f = do
  t <- readProp "text" f
  d <- readDate $ readProp "date" f
  i <- readProp "id" f
  u <- readProp "userId" f
  pure (mkSession d t i u)

instance respondableSession :: Respondable Session where
  responseType = JSONResponse
  fromResponse = read

readDate :: F String -> F Date
readDate (Left s) = 
  Left s
readDate (Right s) = 
  maybe error pure (Date.fromString s) 
  where
    error = Left (TypeMismatch "Date" "asdf")

mkSession :: Date -> String -> Int -> Int -> Session
mkSession d t i u = Session { date: d, text: t, id: i, userId: u }

getSessionDate :: Session -> Date
getSessionDate (Session d) = d.date

getSessionText :: Session -> String
getSessionText (Session s) = s.text

dateIso8601 :: Date -> String
dateIso8601 = renderDate >>> (++ "T00:00:00.000Z")

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

