module Router where

import BigPrelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import Control.Monad.Aff (Aff(), forkAff)
import qualified Control.Monad.Aff as AF
import Routing
import Control.Monad.Eff.Exception
import Control.Monad.Aff.AVar
import DOM

import Routing.Match
import Routing.Match.Class

import qualified Layout as L
import Util

data Input a 
  = GotoSession a
  | GotoProfile a

instance inputFunctor :: Functor Input where
  map f (GotoSession a) = GotoSession (f a)
  map f (GotoProfile a) = GotoProfile (f a)

data Routes
  = ViewProfile
  | LogSession

init :: State
init = { currentPage: "lolo" }

routing :: Match Routes
routing = ViewProfile <$ lit "profile"
      <|> LogSession <$ lit "session"

type State =
  { currentPage :: String 
  }

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render state =
      L.defaultLayout
        [ H.p_ 
          [ H.text "Lorem asdf asdf asdf asdf aosdifuh api9usd8ygfa dofasduiofhyaoi sudhfoa kjndf,kajshcvoiuayhsdofjkqnwefkljhnasodiufyha sdofjkibnasldkfuha so8idfyughaosdk jfnlaskdjfhaoi sduyfhoaisdfnh laksjdhf oaiusdhyfo aisudhfn la;ksdjhf oaiusdy hfaisdf. QuickLift is a quick and easy way to log your weightlifting sessions."
          ]
        , H.h1_ [ H.text (state.currentPage) ]
        ]

    eval :: Eval Input State Input g
    eval (GotoProfile next) = do
      modify (_ { currentPage = "Profile" })
      pure next
    eval (GotoSession next) = do
      modify (_ { currentPage = "Session" })
      pure next

type Effects = (dom :: DOM, avar :: AVAR, err :: EXCEPTION)

routeSignal :: Driver Input Effects
            -> Aff Effects Unit
routeSignal driver = do
  routeTpl <- matchesAff routing
  uncurry (redirects driver) routeTpl


redirects :: Driver Input Effects
          -> Maybe Routes
          -> Routes
          -> Aff Effects Unit
redirects driver _ LogSession = do
  AF.liftEff' (replaceLocation "#/session")
  driver (action GotoSession)
  pure unit

