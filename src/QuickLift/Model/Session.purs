module QuickLift.Model.Session where

import BigPrelude

import Optic.Lens
import Optic.Core

import qualified Data.String as Str
import Control.Monad.Eff.Unsafe

import Data.Foreign hiding (isNull, isArray)
import Data.Foreign.Class
import Data.Generic

import Types.Date

import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Printer

import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response

newtype Session
  = Session
  { date :: Date
  , text :: String
  , id :: Int
  , user :: Int
  }

emptySession :: Session
emptySession = Session
  { date: runPure (unsafeInterleaveEff now)
  , text: ""
  , user: 1
  , id: -1
  }

instance encodeSession :: EncodeJson Session where
  encodeJson (Session s) =
       "text" := s.text
    ~> "date" := toISOString s.date
    ~> "user" := s.user
    ~> "id" := s.id
    ~> jsonEmptyObject


instance requestableSession :: Requestable Session where
  toRequest s =
    let str = printJson (encodeJson s) :: String
     in toRequest str

newtype ArrSession = ArrSession (Array Session)

unArrSession :: ArrSession -> Array Session
unArrSession (ArrSession a) = a

instance isForeignArrSession :: IsForeign ArrSession where
  read f = ArrSession <$> read f

instance respondableArrSession :: Respondable ArrSession where
  responseType = Tuple Nothing JSONResponse
  fromResponse = read

instance sessionIsForeign :: IsForeign Session where
  read = readSession

readSession :: Foreign -> F Session
readSession f = do
  t <- readProp "text" f
  d <- readProp "date" f
  i <- readProp "id" f
  u <- readProp "user" f
  pure (mkSession d t i u)

instance respondableSession :: Respondable Session where
  responseType = Tuple Nothing JSONResponse
  fromResponse = read


mkSession :: Date -> String -> Int -> Int -> Session
mkSession d t i u = Session { date: d, text: t, id: i, user: u }

_Session :: LensP Session { text :: String, date :: Date, user :: Int, id :: Int }
_Session f (Session o) = Session <$> f o

date_ :: forall a b r. Lens { date :: a | r } { date :: b | r } a b
date_ f o = o { date = _ } <$> f o.date

user :: forall a b r. Lens { user :: a | r } { user :: b | r } a b
user f o = o { user = _ } <$> f o.user

text_ :: forall a b r. Lens { text :: a | r } { text :: b | r } a b
text_ f o = o { text = _ } <$> f o.text

id_ :: forall a b r. Lens { id ::  a | r } { id :: b | r } a b
id_ f o = o { id = _ } <$> f o.id
