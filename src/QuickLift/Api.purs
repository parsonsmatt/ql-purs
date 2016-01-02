module QuickLift.Api where

import BigPrelude
import Debug.Trace

import Data.Foreign hiding (isNull, isArray)
import Data.Foreign.Class
import Control.Monad.Aff
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.Affjax as AJ
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader
import Network.HTTP.MimeType
import Data.Int as Int

import QuickLift.Model

getUser :: forall eff. Int -> Aff (ajax :: AJAX | eff) (Maybe User)
getUser i = do
    { response: response } <- AJ.get ("users/" ++ show i)
    pure <<< eitherToMaybe <<< fromResponse $ response

getUserSessions :: forall eff. Int -> Aff (ajax :: AJAX | eff) (Maybe (Array Session))
getUserSessions i = map (map unArrSession) do
    { response: response } <- AJ.get ("users/" ++ show i ++ "/sessions")
    pure <<< eitherToMaybe <<< fromResponse $ response

postSession :: forall eff. Session -> Aff (ajax :: AJAX | eff) (Maybe Int)
postSession s = do
    res <- qlReq "sessions" s
    let str = Int.floor <$> (eitherToMaybe <<< read $ res.response)
    pure str

qlReq :: forall eff r a. (Respondable r, Requestable a)
      => String -> a -> Aff (ajax :: AJAX | eff) (AJ.AffjaxResponse r)
qlReq p r =
    AJ.affjax $ AJ.defaultRequest
                    { url = p
                    , method = POST
                    , headers = [ContentType (MimeType "application/json")]
                    , content = Just r
                    }

postRegistration :: forall eff. UserReg -> Aff (ajax :: AJAX | eff) (Either String Int)
postRegistration u = do
    res <- qlReq "users" u
    pure (either (Left <<< show) id (foreignToEither res.response))

foreignToEither
    :: forall e a
     . (IsForeign a, IsForeign e)
    => Foreign -> F (Either e a)
foreignToEither fgn = Right <$> readProp "Right" fgn <|> Left <$> readProp "Left" fgn

postAuthentication :: forall eff. UserAuth -> Aff (ajax :: AJAX | eff) (Maybe String)
postAuthentication auth = do
    res <- qlReq "users/login" auth
    pure (eitherToMaybe <<< read $ res.response)
