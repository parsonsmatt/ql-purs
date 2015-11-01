module Router where

import BigPrelude

import Halogen
import QuickLift

import DOM (DOM())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Int (floor)
import Routing
import Routing.Match
import Routing.Hash.Aff

import Routing.Match.Class

import Control.Monad.Aff.AVar

import Types
import Control.Monad.Aff (Aff(), forkAff)

routing :: Match Routes
routing = profile
      <|> sessions
      <|> register
      <|> login
      <|> home
  where
    login = Login <$ route "login"
    register = Registration <$ route "register"
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> int
            <|> New <$ lit "new"
            <|> pure Index
    int = floor <$> num

type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver Input eff -> Aff (Effects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  uncurry (redirects driver) (Tuple old new)

redirects :: forall eff. Driver Input eff -> Maybe Routes -> Routes -> Aff (Effects eff) Unit
redirects driver _ =
  driver <<< action <<< Goto

