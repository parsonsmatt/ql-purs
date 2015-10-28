module Main where

import BigPrelude

import Control.Monad.Aff (Aff(), runAff, forkAff)

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)
import Network.HTTP.Affjax (AJAX())

import qualified Router as R
import Types

main :: Eff _ Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI R.ui (installedState R.init)
  appendToBody app.node
  forkAff $ R.routeSignal app.driver
