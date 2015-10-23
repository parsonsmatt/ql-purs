module Component.Sessions where

import Prelude
import Data.Generic

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.Themes.Bootstrap3 as B

import Types

data Input a
  = Noop a
  | Routed CRUD a

type State = { currentCrud :: CRUD }

data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare


ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render st =
      H.div_
        [ H.h1_ [ H.text "Your Sessions" ]
        , H.p_ [ H.text "wow you lift a LOT" ]
        , H.a [ P.href "#/sessions/new", P.classes [B.btn, B.btnDefault] ]
              [ H.text "New Session" ]
        , currentView st
        ]

    currentView st =
      case st.currentCrud of
           Index -> H.p_ [ H.a [ P.href "#/sessions/2" ] [ H.text "Session #2" ] ]
           Show n -> H.p_ [ H.text $ "Checking out session " ++ show n ]
           New -> H.p_ [ H.text "New session... I should be a form or something, right?" ]

    eval :: Eval _ _ _ g
    eval (Noop n) =
      pure n
    eval (Routed crud n) = do
      modify (_{ currentCrud = crud })
      pure n
