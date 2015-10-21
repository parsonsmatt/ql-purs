module Main where

import BigPrelude

import Control.Monad.Aff (runAff)

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)

import qualified Layout as L

import qualified Halogen.HTML.Indexed as H

main :: Eff _ Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui unit
  appendToBody app.node

data Input a = Noop a

ui :: forall g. (Functor g) => Component Unit Input g
ui = component render eval
  where
    render _ =
      L.container 
        [ H.p_ 
          [ H.text "QuickLift is a quick and easy way to log your weightlifting sessions."
          ]
        ]

    eval :: Eval Input Unit Input g
    eval (Noop a) = pure a
