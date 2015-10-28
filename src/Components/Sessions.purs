module Component.Sessions where

import BigPrelude

import Control.Monad
import Data.Functor.Coproduct
import Data.Generic
import Data.Date
import Data.Date.UTC
import Control.Monad.Eff

import Control.Monad.Free (liftFI)

import Halogen
import qualified Halogen.Extra as EX
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.Themes.Bootstrap3 as B

import qualified QuickLift.Api as API
import QuickLift.Model
import Types
import HasLink

import qualified Component.Sessions.New as New

data Input a
  = Routed CRUD a
  | LoadSessions a

type State =
  { currentCrud :: CRUD 
  , loadedSessions :: Maybe (Array Session)
  }

initialState :: forall eff. CRUD -> StateP eff
initialState view = installedState
  { currentCrud: view
  , loadedSessions: Nothing
  }

data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare

type StateP eff = InstalledState State New.State Input New.Input (QLEff eff) New.Slot

type QueryP = Coproduct Input (ChildF New.Slot New.Input)

ui :: forall eff. Component (StateP eff) QueryP (QLEff eff)
ui = parentComponent render eval
  where
    render st =
      H.div_
        [ H.h1_ [ H.text "Your Sessions" ]
        , currentView st.currentCrud st
        ]

    currentView Index st = indexPage st
    currentView (Show n) _ = showPage n
    currentView New _ = EX.slot New.ui New.initialState New.Slot

    eval :: EvalParent Input State New.State Input New.Input (QLEff eff) New.Slot
    eval (Routed crud n) = do
      modify (_{ currentCrud = crud })
      when (crud == Index) (void (pure (action LoadSessions)))
      pure n

    eval (LoadSessions a) = do
      s <- liftH (liftAff' (API.getUserSessions 1))
      modify (_{ loadedSessions = s })
      pure a



indexPage st =
  let sessions = case map linkSession <$> st.loadedSessions of
                      Nothing -> H.p_ [ H.text "No sessions." ]
                      Just s -> H.ul_ (map (H.li_ <<< pure) s)
   in H.div_
    [ newButton
    , loadButton
    , sessions
    ]

loadButton =
  H.a [ E.onClick $ E.input_ LoadSessions ] [ H.text "Loaaaad" ]

linkSession (Session s) =
  H.a [ P.href (link (Sessions (Show 1.0))) ]
    [ H.text (renderDate s.date) ]

showPage :: Number -> _
showPage n =
  H.p_ 
    [ H.text ("Checking out session " ++ show n)
    , newButton
    ]

newButton = 
  H.p_
    [ H.a [ P.href "#/sessions/new", P.classes [B.btn, B.btnDefault] ]
      [ H.text "New Session" ]
    ]
