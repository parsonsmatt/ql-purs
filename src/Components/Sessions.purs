module Component.Sessions where

import Halogen
import qualified Halogen.HTML.Indexed as H

data Input a
  = Noop a

type State = Unit

type Slot = Unit

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render _ =
      H.div_
        [ H.h1_ [ H.text "Your Sessions" ]
        , H.p_ [ H.text "wow you lift a LOT" ]
        ]

    eval :: Eval _ _ _ g
    eval (Noop n) = pure n
