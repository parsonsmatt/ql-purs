module Component.Sessions where

import Prelude
import Data.Generic
import Data.Date
import Data.Date.UTC
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.Themes.Bootstrap3 as B

import Types

data Input a
  = Routed CRUD a
  | Submit String a

type State =
  { currentCrud :: CRUD 
  , currSessionText :: String
  , currSessionDate :: Date
  }

-- yes, I know, whatever, i'm just getting the date
initialState :: State
initialState =
  { currentCrud: Index
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

indexPage =
  H.p_
    [ H.a [ P.href "#/sessions/2" ] 
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
  H.form [ P.classes [ B.formGroup ] ]
    [ H.div [ P.classes [ B.formGroup ] ] 
        [ H.label [ P.class_ B.controlLabel ]
            [ H.text "Session:" ]
        , H.textarea [ P.classes [ B.formControl ] ]
        ]
    , H.div [ P.classes [ B.formGroup ] ]
        [ H.label [ P.class_ B.controlLabel ]
            [ H.text "Date:" ] 
        , H.input [ P.classes [ B.formControl ]
                  , P.value (formDateFromDate $ st.currSessionDate)
                  , P.inputType P.InputDate
                  ]
        ]
    , H.button [ P.classes [B.btn, B.btnPrimary] ] 
        [ H.text "Submit" ]
    ]

formDateFromDate :: Date -> String
formDateFromDate date =
  let y = intFromYear $ year date
      m = monthFromEnum $ month date
      d = intFromDayOfMonth (dayOfMonth date)
   in show y ++ "/" ++ show m ++ "/" ++ show d

intFromDayOfMonth :: DayOfMonth -> Int
intFromDayOfMonth (DayOfMonth d) = d

intFromYear :: Year -> Int
intFromYear (Year i) = i

monthFromEnum :: Month -> Int
monthFromEnum January   = 0
monthFromEnum February  = 1
monthFromEnum March     = 2
monthFromEnum April     = 3
monthFromEnum May       = 4
monthFromEnum June      = 5
monthFromEnum July      = 6
monthFromEnum August    = 7
monthFromEnum September = 8
monthFromEnum October   = 9
monthFromEnum November  = 10
monthFromEnum December  = 11
