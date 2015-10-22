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
  = Goto Routes a

data Routes
  = Profile
  | Session
  | Home

init :: State
init = { currentPage: "Home" }

routing :: Match Routes
routing = Profile <$ lit "" <* lit "profile"
      <|> Session <$ lit "" <* lit "session"
      <|> Home <$ lit ""

type State =
  { currentPage :: String 
  }

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render state =
      L.defaultLayout
        [ H.h1_ [ H.text (state.currentPage) ]
        , H.p_ 
          [ H.text "Lorem asdf asdf asdf asdf aosdifuh api9usd8ygfa dofasduiofhyaoi sudhfoa kjndf,kajshcvoiuayhsdofjkqnwefkljhnasodiufyha sdofjkibnasldkfuha so8idfyughaosdk jfnlaskdjfhaoi sduyfhoaisdfnh laksjdhf oaiusdhyfo aisudhfn la;ksdjhf oaiusdy hfaisdf. QuickLift is a quick and easy way to log your weightlifting sessions."
          ]
        ]

    eval :: Eval Input State Input g
    eval (Goto Profile next) = do
      modify (_ { currentPage = "Profile" })
      pure next
    eval (Goto Session next) = do
      modify (_ { currentPage = "Session" })
      pure next
    eval (Goto Home next) = do
      modify (_ { currentPage = "Home" })
      pure next

type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)
type DriverEffects e = (dom :: DOM | e)

routeSignal :: forall eff. Driver Input (eff)
            -> Aff (err :: EXCEPTION, avar :: AVAR, dom :: DOM | eff) Unit
routeSignal driver = do
  --trace "asdfasdfasdf" \_ -> pure unit
  Tuple old new <- matchesAff routing
  pure unit
  redirects driver old new

redirects :: forall eff. Driver Input (eff)
          -> Maybe Routes
          -> Routes
          -> Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | eff) Unit
redirects driver _ Session = do
  driver (action (Goto Session))
redirects driver _ Profile = do
  driver (action (Goto Profile))
redirects driver _ Home = do
  driver (action (Goto Home))
