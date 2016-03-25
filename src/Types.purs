module Types where

import Prelude (class Eq, Unit, ($), show, (++), (<<<), (==))

import Data.Generic (class Generic, gEq)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Browser.WebStorage as WS

import Data.String (drop)
import DOM (DOM)
import Halogen (HTML, HalogenEffects, Component)
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())
import Control.Monad.Eff.Console (CONSOLE)
import Routing.Hash.Aff (setHash)


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
    | Registration
    | Login
    | Logout

derive instance genericRoutes :: Generic Routes
derive instance genericCrud :: Generic CRUD

instance eqRoute :: Eq Routes where eq = gEq

updateUrl :: forall e. Routes -> Aff (dom :: DOM | e) Unit
updateUrl = setHash <<< drop 1 <<< link

type ComponentSlot s f g = Unit -> { component :: Component s f g, initialState :: s }

type QLEff eff = Aff (QL eff)
type QL eff =
    HalogenEffects (webStorage :: WS.WebStorage
                   , ajax :: AJAX
                   , console :: CONSOLE | eff
                   )

class HasLink a where
    link :: a -> String

instance routesHasLink :: HasLink Routes where
    link Profile = "#/profile"
    link (Sessions crud) = "#/sessions" ++ link crud
    link Home = "#/"
    link Registration = "#/register"
    link Login = "#/login"
    link Logout = "#/logout"

instance crudHasLink :: HasLink CRUD where
    link Index = ""
    link New = "/new"
    link (Show n) = "/" ++ show n

divide :: forall a b. (HasLink a, HasLink b) => (a -> b) -> a -> b
divide = ($)

infixr 7 divide as </>

linkTo :: Routes -> String -> HTML _ _
linkTo r t = H.a [ P.href (link r) ] [ H.text t ]
