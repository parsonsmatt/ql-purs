module Router where

import BigPrelude

import Data.Int hiding (fromString)
import Data.Functor.Coproduct (Coproduct(..), left)
import Control.Monad.Aff (Aff(), forkAff)
import qualified Control.Monad.Aff as AF
import Control.Monad.Eff.Exception
import Control.Monad.Aff.AVar
import DOM
import Control.Monad.Free (liftFI)
import Data.Date

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import Halogen.Component.ChildPath (ChildPath(), cpR, cpL, (:>))

import Routing
import Routing.Match
import Routing.Match.Class

import qualified Layout as L

import qualified Component.Profile as Profile
import qualified Component.Sessions as Sessions
import qualified Component.Home as Home

import Types

data Input a 
  = Goto Routes a

init :: State
init = { currentPage: Home }

routing :: Match Routes
routing = profile
      <|> sessions
      <|> home
  where
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> int
            <|> New <$ lit "new"
            <|> pure Index
    int = floor <$> num

type State =
  { currentPage :: Routes
  }

type ChildState eff =
  Either Profile.State (Either Home.State (Sessions.StateP eff))
type ChildQuery =
  Coproduct Profile.Input (Coproduct Home.Input Sessions.QueryP)
type ChildSlot =
  Either Profile.Slot (Either Home.Slot Sessions.Slot)

pathToProfile :: forall eff. ChildPath Profile.State (ChildState eff) Profile.Input ChildQuery Profile.Slot ChildSlot
pathToProfile = cpL

pathToSessions :: forall eff. ChildPath (Sessions.StateP eff) (ChildState eff) Sessions.QueryP ChildQuery Sessions.Slot ChildSlot
pathToSessions = cpR :> cpR

pathToHome :: forall eff. ChildPath Home.State (ChildState eff) Home.Input ChildQuery Home.Slot ChildSlot
pathToHome = cpR :> cpL

type StateP eff
  = InstalledState State (ChildState eff) Input ChildQuery (QLEff eff) ChildSlot

type QueryP
  = Coproduct Input (ChildF ChildSlot ChildQuery)

ui :: forall eff. Component (StateP eff) QueryP (QLEff eff)
ui = parentComponent render eval
  where
    render state =
      L.defaultLayout
        [ viewPage state.currentPage
        ]

    viewPage :: Routes -> HTML (SlotConstructor (ChildState eff) ChildQuery (QLEff eff) ChildSlot) Input
    viewPage (Sessions view) =
      H.slot' pathToSessions Sessions.Slot \_ -> { component: Sessions.ui, initialState: Sessions.initialState view }
    viewPage Profile =
      H.slot' pathToProfile Profile.Slot Profile.mount
    viewPage Home =
      H.slot' pathToHome Home.Slot Home.mount

    eval :: EvalParent Input State (ChildState eff) Input ChildQuery (QLEff eff) ChildSlot
    eval (Goto route next) = do
      modify (_ { currentPage = route })
      handleRoute route
      pure next

    handleRoute (Sessions view) = void $
      query' pathToSessions Sessions.Slot (action (left <<< Sessions.Routed view))
    handleRoute _ =
      pure unit

type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver QueryP eff
            -> Aff (Effects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. Driver QueryP eff
          -> Maybe Routes
          -> Routes
          -> Aff (Effects eff) Unit
redirects driver _ =
  driver <<< left <<< action <<< Goto
