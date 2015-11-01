module Main where

import BigPrelude

import Control.Monad.Aff (Aff(), runAff, forkAff)
import Control.Monad.Eff.Console (log)

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)
import Network.HTTP.Affjax (AJAX())

import qualified QuickLift as Q
import qualified QuickLift.State as S
import qualified Router as R
import Types

main :: forall eff. Eff (QL eff) Unit
main = runAff (log <<< show) (const (pure unit)) do
  app <- runUI Q.ui S.initialState
  appendToBody app.node
  forkAff $ R.routeSignal app.driver
