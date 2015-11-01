module QuickLift where

import BigPrelude

import Data.Foldable
import qualified Data.String as Str

import Data.Int hiding (fromString)
import Data.Functor.Coproduct (Coproduct(..), left)
import Control.Monad
import Data.Array hiding ((..))
import Control.Monad.Eff.Console
import qualified Routing.Hash.Aff as R

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.Themes.Bootstrap3 as B

import qualified Form as F
import qualified Form.AForm as AF
import qualified Form.WForm as WF
import Form.Types (FormInput(..))

import QuickLift.View
import QuickLift.State
import QuickLift.Model
import QuickLift.Input
import qualified QuickLift.Api as API

import qualified Layout as L

import Types
import Types.Date



ui :: forall eff. Component State Input (QLEff eff)
ui = component render eval
  where
    render state =
      L.defaultLayout
        [ renderView state.currentPage state
        ]

    eval :: Eval Input State Input (QLEff eff)
    eval (Goto route next) = do
      modify (_ { currentPage = route })
      case route of
           Registration -> modify (stCurrentUser .~ Just emptyUser)
           Sessions Index -> eval (LoadSessions unit)
           _ -> pure unit
      pure next

    eval (GetUser i n) = do
      newUser <- liftAff' $ API.getUser i
      modify (_{ currentUser = newUser })
      pure n

    eval (LoadSessions a) = do
      s <- liftAff' (API.getUserSessions 1)
      modify (stLoadedSessions .~ (concat $ maybeToArray s))
      pure a

    eval (NewSession inp a) = do
      handleNewSession inp
      pure a

    eval (Register inp a) = do
      handleRegistration inp
      pure a

    eval (Authenticate inp a) = handleAuthentication inp $> a

    handleNewSession Submit = do
      sess <- gets _.currentSession
      result <- liftAff' (API.postSession sess)
      for_ result \n -> do
        let saved' = sess # _Session .. id_ .~ n
            rt = Sessions </> Show n
        modify (stCurrentSession .~ saved')
        modify (stLoadedSessions %~ (saved' :))
        eval (Goto rt unit)
        liftAff' (updateUrl rt)

    handleNewSession (Edit fn) = do
      modify (stCurrentSession %~ fn)

    handleRegistration (Edit fn) = do
      modify (stRegistration %~ fn)

    handleRegistration Submit = do
      reg <- gets _.registration
      res <- liftAff' (API.postRegistration reg)
      for_ res \n -> do
        let saved = emptyUser # (_User .. name .~ (reg ^. _UserReg .. name))
                              # (_User .. email .~ (reg ^. _UserReg .. email))
                              # (_User .. id_ .~ n)
        modify (stCurrentUser ?~ saved)
        eval (Goto Profile unit)
        liftAff' (updateUrl Profile)

    handleAuthentication (Edit fn) = modify (stAuthentication %~ fn)

    handleAuthentication Submit = do
      auth <- gets _.authentication
      res <- liftAff' (API.postAuthentication auth)
      liftEff' (log .. show $ res)
      for_ res \user -> do
        modify (stCurrentUser ?~ user)
        eval (Goto Profile unit)
        liftAff' (updateUrl Profile)
