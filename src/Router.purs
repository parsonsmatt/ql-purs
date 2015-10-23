module Router where

import BigPrelude

import Data.Functor.Coproduct (Coproduct(..), left)
import Control.Monad.Aff (Aff(), forkAff)
import qualified Control.Monad.Aff as AF
import Control.Monad.Eff.Exception
import Control.Monad.Aff.AVar
import DOM
import Control.Monad.Free (liftFI)

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
    parseCRUD = Show <$> num 
            <|> New <$ lit "new"
            <|> pure Index

type State =
  { currentPage :: Routes
  }

type ChildState = Either Profile.State (Either Home.State Sessions.State)
type ChildQuery = Coproduct Profile.Input (Coproduct Home.Input Sessions.Input)
type ChildSlot = Either Profile.Slot (Either Home.Slot Sessions.Slot)

pathToProfile :: ChildPath Profile.State ChildState Profile.Input ChildQuery Profile.Slot ChildSlot
pathToProfile = cpL

pathToSessions :: ChildPath Sessions.State ChildState Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cpR :> cpR

pathToHome :: ChildPath Home.State ChildState Home.Input ChildQuery Home.Slot ChildSlot
pathToHome = cpR :> cpL

type StateP g
  = InstalledState State ChildState Input ChildQuery g ChildSlot

type QueryP
  = Coproduct Input (ChildF ChildSlot ChildQuery)

ui :: forall eff. Component (StateP (QLEff eff)) QueryP (QLEff eff)
ui = parentComponent render eval
  where
    render state =
      L.defaultLayout
        [ viewPage state.currentPage
        ]

    viewPage :: Routes -> HTML (SlotConstructor ChildState ChildQuery (QLEff eff) ChildSlot) Input
    viewPage (Sessions view) =
      H.slot' pathToSessions Sessions.Slot \_ -> { component: Sessions.ui, initialState: { currentCrud: view } }
    viewPage Profile =
      H.slot' pathToProfile Profile.Slot Profile.mount
    viewPage Home =
      H.slot' pathToHome Home.Slot Home.mount

    eval :: EvalParent Input State ChildState Input ChildQuery (QLEff eff) ChildSlot
    eval (Goto Profile next) = do
      modify (_ { currentPage = Profile })
      pure next
    eval (Goto (Sessions view) next) = do
      modify (_ { currentPage = Sessions view })
      query' pathToSessions Sessions.Slot (action (Sessions.Routed view))
      pure next
    eval (Goto Home next) = do
      modify (_ { currentPage = Home })
      pure next

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
-- redirects driver _ Home = 
--   driver (left (action (Goto Home))))
-- redirects driver _ Profile =
--   driver (left (action (Goto Profile))))
-- redirects driver _ (Sessions view) =
--   driver (left (action (Goto (Sessions view)))))
