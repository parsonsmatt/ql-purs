module Types where

import Prelude

import Halogen
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())

data CRUD
  = Index
  | Show Number
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
type QLEff eff = Aff (HalogenEffects (ajax :: AJAX | eff)) 
type QL eff = HalogenEffects (ajax :: AJAX | eff)

-- link :: Routes -> String
-- link Profile = "#/profile"
-- link (Sessions view) = "#/sessions" ++ linkCrud view
-- link Home = "#/"
-- 
-- linkCrud :: CRUD -> String
-- linkCrud Index = ""
-- linkCrud New = "/new"
-- linkCrud (Show n) = "/" ++ show n
