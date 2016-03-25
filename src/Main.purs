module Main where

import BigPrelude (Unit, Eff, ($), bind, unit, pure, const)

import Control.Monad.Aff (forkAff, runAff)
import Control.Monad.Eff.Exception (throwException)

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
    runAff throwException (const (pure unit)) do
        app <- runUI Q.ui S.initialState { authToken = token }
        appendToBody app.node
        forkAff $ R.routeSignal app.driver
