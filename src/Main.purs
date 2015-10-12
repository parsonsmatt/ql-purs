module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Aff (runAff)

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)

import qualified Halogen.HTML.Indexed as H

main :: forall eff. Eff _ Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui unit
  appendToBody app.node

data Input a = Noop a

ui :: forall g p. (Functor g) => Component Unit Input g p
ui = component render eval
  where
    render _ =
      H.div_ 
        [ H.h1_ [ H.text "QuickLift" ]
        , H.p_ [ H.text "QuickLift is a quick and easy way to log your weightlifting sessions." ]
        ]
    eval :: Eval _ _ _ g
    eval (Noop a) = pure a
