module Router where

import BigPrelude

import Debug.Trace
import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import Control.Monad.Aff (Aff(), forkAff)
import qualified Control.Monad.Aff as AF
import Routing
import Control.Monad.Eff.Exception
import Control.Monad.Aff.AVar
import DOM
import Control.Monad.Free (liftFI)

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
        , H.p [ E.onClick $ E.input_ GotoSession ] [ H.text "asdf" ]
        ]

    eval :: Eval Input State Input g
    eval (GotoProfile next) = do
      modify (_ { currentPage = "Profile" })
      trace "wtf" \_ -> pure unit
      pure next
    eval (GotoSession next) = do
      trace "wtf" \_ -> pure unit
      modify (_ { currentPage = "Session" })
      pure next

type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)
type DriverEffects e = (dom :: DOM | e)

routeSignal :: forall eff. Driver Input (eff)
            -> Aff (err :: EXCEPTION, avar :: AVAR, dom :: DOM | eff) Unit
routeSignal driver = do
  trace "asdfasdfasdf" \_ -> pure unit
  Tuple old new <- matchesAff routing
  redirects driver old new
  pure unit


redirects :: forall eff. Driver Input (eff)
          -> Maybe Routes
          -> Routes
          -> Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | eff) Unit
redirects driver _ LogSession = do
  replaceLocation "#/session"
  driver (action GotoSession)
  pure unit
redirects driver _ ViewProfile = do
  replaceLocation "#/profile"
  driver (action GotoProfile)
  pure unit
