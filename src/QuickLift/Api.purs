module QuickLift.Api where

import BigPrelude

import Control.Monad.Aff
-- import Data.Argonaut.Core
-- import Data.Argonaut.Decode
import Data.Foreign.Class
import Data.Foreign hiding (isNull, isArray)
import Data.Int as Int
import Network.HTTP.Affjax
import Network.HTTP.Affjax as AJ
-- import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
-- import Network.HTTP.Method
-- import Network.HTTP.MimeType
-- import Network.HTTP.RequestHeader

import QuickLift.Api.Util
import QuickLift.Model

getUser :: forall eff. Int -> Aff (ajax :: AJAX | eff) (Maybe User)
getUser i = do
    { response: response } <- AJ.get ("users/" ++ show i)
    pure <<< eitherToMaybe <<< fromResponse $ response

getUserSessions :: forall eff. User -> Aff (ajax :: AJAX | eff) (Maybe (Array Session))
getUserSessions (User user) = map (map unArrSession) do
    { response: response } <- AJ.get ("lifters/" <> user.name <> "/sessions")
    pure <<< eitherToMaybe <<< fromResponse $ response

postSession :: forall eff. String -> User -> Session -> Aff (ajax :: AJAX | eff) (Maybe Int)
postSession token (User user) s = do
    res <- qlAuth token ("lifters/" <> user.name <> "/sessions") s
    let str = Int.floor <$> (eitherToMaybe <<< joinForeign show $ res.response)
    pure str

postRegistration
    :: forall eff
     . UserReg
    -> Aff (ajax :: AJAX | eff) (Either String AuthResponse)
postRegistration u = do
    { response: res } <- qlPost "users" u
    pure (joinForeign show res)


postAuthentication :: forall eff. UserAuth -> Aff (ajax :: AJAX | eff) (Maybe (Tuple String User))
postAuthentication auth = do
    { response: res } <- qlPost "users/login" auth
    let parsed = Tuple <$> readProp "sessionId" res <*> readProp "person" res
    pure (eitherToMaybe $ parsed)

verifySession
    :: forall eff
     . String
    -> Aff (ajax :: AJAX | eff) (Maybe (Tuple String User))
verifySession token = do
    { response: res } <- qlPost "users/verify" (show token)
    pure <<< eitherToMaybe $
        Tuple <$> readProp "sessionId" res <*> readProp "person" res
