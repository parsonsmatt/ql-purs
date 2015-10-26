module Component.Sessions where

import BigPrelude
import Data.Generic
import Data.Date
import Data.Date.UTC
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Unsafe.Coerce

import Debug.Trace
import Control.Monad.Free (liftFI)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.Themes.Bootstrap3 as B

import Types
import HasLink

import qualified Form as F

data Input a
  = Routed CRUD a
  | Submit String a

type State =
  { currentCrud :: CRUD 
  , currSessionText :: String
  , currSessionDate :: Date
  }

-- yes, I know, whatever, i'm just getting the date
initialState :: CRUD -> State
initialState view =
  { currentCrud: view
  , currSessionText: ""
  , currSessionDate: runPure (unsafeInterleaveEff now)
  }

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
        , currentView st.currentCrud st
        ]

    currentView Index _ = indexPage
    currentView (Show n) _ = showPage n
    currentView New st = sessionForm st

    eval :: Eval Input State Input g
    eval (Routed crud n) = do
      modify (_{ currentCrud = crud })
      pure n
    eval (Submit str n) = do
      trace str (\_ -> pure unit)
      pure n


indexPage =
  H.p_
    [ H.a [ P.href (link $ Sessions $ Show 2.0) ] 
          [ H.text "Session #2" ]
    , newButton
    ]

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

sessionForm :: State -> _
sessionForm st =
  F.form (\e -> trace (unsafeCoerce e) (\_ -> pure unit) *> (Submit "lolo") )
    [ F.textAreaFieldView "session" "Session:" [] (Right st.currSessionText) true
    , F.dateFieldView "date" "Date:" [] (Right st.currSessionDate) true
    ]
