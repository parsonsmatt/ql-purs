module QuickLift.Api where

import BigPrelude

import Data.Foreign.Class
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())
import qualified Network.HTTP.Affjax as AJ
import Network.HTTP.Affjax.Response

import QuickLift.Model

getUser :: forall eff. Int -> Aff (ajax :: AJAX | eff) (Maybe User)
getUser i = do
  { response: response } <- AJ.get ("users/" ++ show i)
  pure <<< eitherToMaybe <<< fromResponse $ response