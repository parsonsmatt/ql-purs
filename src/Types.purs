module Types where

import Prelude

import Halogen
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())

data CRUD
  = Index
  | Show Number
  | New

data Routes
  = Profile
  | Sessions CRUD
  | Home

type ComponentSlot s f g = Unit -> { component :: Component s f g, initialState :: s } 

type QLEff eff = Aff (HalogenEffects (ajax :: AJAX | eff)) 
type QL eff = HalogenEffects (ajax :: AJAX | eff)
