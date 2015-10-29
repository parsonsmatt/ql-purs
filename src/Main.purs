module Main where

import BigPrelude

import Unsafe.Coerce

import Control.Monad.Aff (Aff(), runAff, forkAff)
import Control.Monad.Eff.Console

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)
import Network.HTTP.Affjax (AJAX())

import qualified Router as R
import Types

main :: forall eff. Eff (QL eff) Unit
main = runAff (log <<< show) (const (pure unit)) do
  app <- runUI R.ui (installedState R.init)
  appendToBody app.node
  forkAff $ R.routeSignal app.driver
