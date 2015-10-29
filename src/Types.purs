module Types where

import Prelude

import Halogen
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())
import Control.Monad.Eff.Console

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

type ComponentSlot s f g = Unit -> { component :: Component s f g, initialState :: s } 

type QLApp = QLEff ()
type QLEff eff = Aff (QL eff) 
type QL eff = HalogenEffects (ajax :: AJAX, console :: CONSOLE | eff)
