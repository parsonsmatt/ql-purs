module Main where

import BigPrelude

import Control.Monad.Aff (runAff, forkAff)
import Control.Monad.Eff.Console (log)

import Browser.WebStorage as WS

import Halogen (runUI)
import Halogen.Util (appendToBody)

import QuickLift as Q
import QuickLift.State as S
import Router as R
import Types (QL())

main :: forall eff. Eff (QL eff) Unit
main = do
    token <- WS.getItem WS.localStorage "auth"
    runAff (log <<< show) (const (pure unit)) do
        app <- runUI Q.ui S.initialState { authToken = token }
        appendToBody app.node
        forkAff $ R.routeSignal app.driver
