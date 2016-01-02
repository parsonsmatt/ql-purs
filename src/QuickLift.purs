module QuickLift where

import BigPrelude

import qualified Data.String as Str

import Data.Int hiding (fromString)
import Control.Monad
import Data.Array hiding ((..))
import Control.Monad.Eff.Console as Console

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Events.Indexed as E
import Halogen.Themes.Bootstrap3 as B

import Form.Types (FormInput(..))

import QuickLift.View
import QuickLift.State
import QuickLift.Model
import QuickLift.Input
import QuickLift.Api as API

import Layout as L

import Types

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
          liftAff' (updateUrl route)
          pure next

      eval (GetUser i n) = do
          newUser <- liftAff' $ API.getUser i
          modify (_{ currentUser = newUser })
          pure n

      eval (LoadSessions a) = do
          s <- liftAff' (API.getUserSessions 1)
          modify .. set stLoadedSessions .. concat .. maybeToArray $ s
          pure a

      eval (NewSession inp a) = handleNewSession inp $> a
      eval (Register inp a) = handleRegistration inp $> a
      eval (Authenticate inp a) = handleAuthentication inp $> a

      handleNewSession (Edit fn) = modify (stCurrentSession %~ fn)
      handleNewSession Submit = do
          sess <- gets _.currentSession
          result <- liftAff' (API.postSession sess)
          for_ result \n -> do
              let saved' = sess # _Session .. id_ .~ n
                  rt = Sessions </> Show n
              modify (stCurrentSession .~ saved')
              modify (stLoadedSessions %~ (saved' :))
              eval (Goto rt unit)

      handleRegistration (Edit fn) = modify (stRegistration %~ fn)
      handleRegistration Submit = do
          reg <- gets _.registration
          res <- liftAff' (API.postRegistration reg)
          case res of
               Right n -> do
                   let saved = User { name:  reg ^. _UserReg .. name
                                    , email: reg ^. _UserReg .. email
                                    , id:    n
                                    }
                   modify (stCurrentUser ?~ saved)
                   eval (Goto Profile unit)
               Left err -> do
                   modify (stErrors ?~ ["There was an error registering:", err])

      handleAuthentication (Edit fn) = modify (stAuthentication %~ fn)
      handleAuthentication Submit = do
          auth <- gets _.authentication
          res <- liftAff' (API.postAuthentication auth)
          liftEff' (Console.log .. show $ res)
          case res of
               Nothing -> do
                   modify (stErrors ?~ ["That login wasn't quite right. Try again?"])
               Just token -> do
                   modify (stAuthToken ?~ token)
                   modify (stErrors ?~ [])
                   eval (Goto Profile unit)
