module Component.Sessions.New where

import BigPrelude
import Control.Monad

import qualified Data.Date as D
import qualified Data.Date.UTC as D
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
import HasLink

data Slot = Slot

derive instance slotGeneric :: Generic Slot
instance eqSlot :: Eq Slot where eq = gEq
instance ordGeneric :: Ord Slot where compare = gCompare

type State =
  { currentSession :: Session
  }

initialState :: State
initialState =
  { currentSession: Session
    { date: runPure (unsafeInterleaveEff D.now)
    , text: ""
    }
  }

data Input a
  = Submit a
  | EditDate String a
  | EditText String a

ui :: forall eff. Component State Input QLApp
ui = component render eval
  where
    render st = 
       F.form Submit
         [ F.textarea "session" "Session:" (getSessionText st.currentSession) EditText
         , F.date "date" "Date:" (renderDate <<< getSessionDate $ st.currentSession) EditDate
         ]

    eval :: Eval _ _ _ QLApp
    eval (Submit a) = do
      st <- get
      liftAff' (postSession st.currentSession)
      pure a

    eval (EditDate str a) = do
      { currentSession: Session s } <- get
      let d = fromMaybe s.date (D.fromString str)
      when (s.date /= d) (modify (_ { currentSession = Session (s { date = d })}))
      pure a

    eval (EditText str a) = do
      { currentSession: Session s } <- get
      modify (_ { currentSession = Session (s { text = str })})
      pure a
