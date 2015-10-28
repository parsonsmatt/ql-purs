module Component.Profile where

import BigPrelude
import Data.Generic
import Control.Monad.Aff (Aff())

import Halogen
import qualified Halogen.Extra as EX
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.Themes.Bootstrap3 as B

import QuickLift.Model
import qualified QuickLift.Api as API

import HasLink
import Types

data Input a
  = GetUser Int a

type State = { user :: Maybe User }

initialState :: State
initialState = { user: Nothing }

data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare

mount :: forall eff. ComponentSlot State Input (QLEff eff)
mount = EX.mount ui initialState

ui :: forall eff. Component State Input (QLEff eff)
ui = component render eval
  where
    render st =
      H.div_
        [ H.h1_ [ H.text "Home" ]
        , H.p_ [ H.text "what a nice profile!" ]
        , H.div_ (printUser st.user)
        , H.a [ E.onClick $ E.input_ (GetUser 1) ] 
              [ H.text "Get a user maybe?" ]
        ]

    printUser Nothing = []
    printUser (Just (User user)) =
      [ H.p_ [ H.text ("It's " <> user.name <> "!") ]
      , H.a [ P.href (link $ Sessions Index) ]
        [ H.text "Go to sessions" ]
      ]

    eval :: Eval _ _ _ (QLEff eff)
    eval (GetUser i n) = do
      newUser <- liftAff' $ API.getUser i
      modify (_{ user = newUser })
      pure n
