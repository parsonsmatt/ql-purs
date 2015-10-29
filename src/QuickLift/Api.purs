module QuickLift.Api where

import BigPrelude
import Debug.Trace

import Data.Foreign.Class
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Method (Method(..))
import qualified Network.HTTP.Affjax as AJ
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader
import Network.HTTP.MimeType
import Data.Int

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
  res <- AJ.affjax (AJ.defaultRequest 
                      { url = "sessions"
                      , method = POST
                      , headers = [ContentType (MimeType "application/json")]
                      , content = Just s
                      }
                    )
  let str = floor <$> (eitherToMaybe <<< read $ res.response)
  pure str
