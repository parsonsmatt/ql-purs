module Router where

import BigPrelude

import Halogen

import DOM (DOM())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Int (floor)

import Routing
import Routing.Match
import Routing.Hash.Aff

import Routing.Match.Class

import Control.Monad.Aff.AVar

import QuickLift.Input
import Types
import Control.Monad.Aff (Aff(), forkAff)

routing :: Match Routes
routing = profile
      <|> sessions
      <|> register
      <|> login
      <|> logout
      <|> home
  where
    login = Login <$ route "login"
    logout = Logout <$ route "logout"
    register = Registration <$ route "register"
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> int
            <|> New <$ lit "new"
            <|> pure Index
    int = floor <$> num

type Routing e = Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver Input eff -> Routing eff Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  uncurry (redirects driver) (Tuple old new)

redirects :: forall eff. Driver Input eff -> Maybe Routes -> Routes -> Routing eff Unit
redirects driver _ =
  driver <<< action <<< Goto
