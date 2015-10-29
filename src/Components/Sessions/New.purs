module Component.Sessions.New where

import BigPrelude
import Control.Monad

import Data.Generic

import qualified Data.String as Str

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.Themes.Bootstrap3 as B

import Control.Monad.Eff.Console
import Control.Monad.Eff.Unsafe

import Debug.Trace

import qualified Form as F
import QuickLift.Model
import QuickLift.Api

import Types
import Types.Date

import HasLink

data Slot = Slot

derive instance slotGeneric :: Generic Slot
instance eqSlot :: Eq Slot where eq = gEq
instance ordGeneric :: Ord Slot where compare = gCompare

type State =
  { currentSession :: Session
  , success :: Maybe Int
  }

initialState :: State
initialState =
  { currentSession: Session
    { date: runPure (unsafeInterleaveEff now)
    , text: ""
    , userId: 1
    , id: -1
    }
  , success: Nothing
  }

data Input a
  = Submit a
  | EditDate String a
  | EditText String a

ui :: forall eff. Component State Input (QLEff eff)
ui = component render eval
  where
    render st = 
      H.div_ 
        [ F.form Submit
          [ F.textarea "session" "Session:" (getSessionText st.currentSession) EditText
          , F.date "date" "Date:" (yyyy_mm_dd <<< getSessionDate $ st.currentSession) EditDate
          ]
        , succLink st.success
        ]

    succLink Nothing =
      H.p_ [ H.text "lol" ]
    succLink (Just n) =
      H.a [ P.href (link (Sessions $ Show n)) ]
        [ H.text "asdfffff" ]


    eval :: Eval _ _ _ (QLEff eff)
    eval (Submit a) = do
      st <- get
      result <- liftAff' (postSession st.currentSession)
      modify (_{ success = result })
      liftEff' (log (show result))
      pure a

    eval (EditDate str a) = do
      { currentSession: Session s } <- get
      let d = fromMaybe s.date (dateFromString str)
      when (s.date /= d) (modify (_ { currentSession = Session (s { date = d })}))
      pure a

    eval (EditText str a) = do
      { currentSession: Session s } <- get
      modify (_ { currentSession = Session (s { text = str })})
      pure a
