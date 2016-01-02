module QuickLift.Api where

import BigPrelude

import Control.Monad.Aff
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Foreign
import Data.Foreign.Class
import Data.Int
import Network.HTTP.Affjax
import Network.HTTP.Affjax as AJ
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.RequestHeader

import QuickLift.Api.Util
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
    let str = floor <$> (eitherToMaybe <<< read $ res.response)
    pure str

qlReq :: forall eff r a. (Respondable r, Requestable a)
      => String -> a -> Aff (ajax :: AJAX | eff) (AJ.AffjaxResponse r)
qlReq p r =
    AJ.affjax $
        AJ.defaultRequest
            { url = p
            , method = POST
            , headers = [ContentType (MimeType "application/json")]
            , content = Just r
            }

postRegistration :: forall eff. UserReg -> Aff (ajax :: AJAX | eff) (Either String Int)
postRegistration u = do
    { response: res } <- qlReq "users" u
    pure $ joinForeign show res

postAuthentication :: forall eff. UserAuth -> Aff (ajax :: AJAX | eff) (Maybe (Tuple String User))
postAuthentication auth = do
    { response: res } <- qlReq "users/login" auth
    let parsed = Tuple <$> readProp "sessionId" res <*> readProp "person" res
    pure (eitherToMaybe $ parsed)

