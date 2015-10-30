module Types where

import Prelude

import Data.String (drop)
import DOM
import Halogen
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())
import Control.Monad.Eff.Console
import Routing.Hash.Aff

data CRUD
  = Index
  | Show Int
  | New

instance eqCrud :: Eq CRUD where
  eq Index Index = true
  eq New New = true
  eq (Show a) (Show b) = a == b
  eq _ _ = false

data Routes
  = Profile
  | Sessions CRUD
  | Home

updateUrl :: forall e. Routes -> Aff (dom :: DOM | e) Unit
updateUrl = setHash <<< drop 1 <<< link

type ComponentSlot s f g = Unit -> { component :: Component s f g, initialState :: s } 

type QLEff eff = Aff (QL eff) 
type QL eff = HalogenEffects (ajax :: AJAX, console :: CONSOLE | eff)

class HasLink a where
  link :: a -> String

instance routesHasLink :: HasLink Routes where
  link Profile = "#/profile"
  link (Sessions crud) = "#/sessions" ++ link crud
  link Home = "#/"

instance crudHasLink :: HasLink CRUD where
  link Index = ""
  link New = "/new"
  link (Show n) = "/" ++ show n

(</>) :: forall a b. (HasLink a, HasLink b) => (a -> b) -> a -> b
(</>) = ($)
