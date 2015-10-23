module Component.Home where

import BigPrelude
import Data.Generic

import Halogen
import qualified Halogen.Extra as E
import qualified Halogen.HTML.Indexed as H

import Types

data Input a
  = Noop a

type State = Unit

initialState :: Unit
initialState = unit

data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare

mount :: forall g. (Functor g) => ComponentSlot State Input g
mount = E.mount ui initialState

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render _ =
      H.div_
        [ H.h1_ [ H.text "QuickLift" ]
        , H.p_ [ H.text "Welcome to QuickLift" ]
        ]

    eval :: Eval _ _ _ g
    eval (Noop n) = pure n
