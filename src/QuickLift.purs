module QuickLift where

import BigPrelude

import Control.Monad
import Data.Array hiding ((..))
import Data.Int hiding (fromString)

import Browser.WebStorage as WS
import Halogen hiding (set)

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
          L.defaultLayout state
              [ renderView state.currentPage state
              ]

      eval :: Eval Input State Input (QLEff eff)
      eval (Goto route next) = do
          modify (_ { currentPage = route })

          case route of
               Registration -> modify (stCurrentUser .~ Just emptyUser)
               Sessions Index -> eval (LoadSessions next) $> unit
               Logout -> do
                   liftEff' (WS.removeItem WS.localStorage "auth")
                   eval (UserLogout next) $> unit
               _ -> pure unit

          st <- get

          let u = st.currentUser

          unless (isJust u) do
              for_ st.authToken \auth -> do
                  res <- liftAff' (API.verifySession auth)
                  case res of
                       Nothing ->
                           modify (stAuthToken .~ Nothing)
                       Just (Tuple session user) -> do
                           liftEff' (WS.setItem WS.localStorage "auth" session)
                           modify (stErrors ?~ [])
                           modify (stCurrentUser ?~ user)
                           modify (stAuthToken ?~ session)

          liftAff' (updateUrl route)

          pure next

      eval (GetUser i n) = do
          newUser <- liftAff' $ API.getUser i
          modify (_{ currentUser = newUser })
          pure n

      eval (LoadSessions a) = do
          u <- gets _.currentUser
          case u of
               Nothing -> pure a
               Just user -> do
                   s <- liftAff' (API.getUserSessions user)
                   modify .. set stLoadedSessions .. concat .. maybeToArray $ s
                   pure a

      eval (NewSession inp a) = handleNewSession inp $> a
      eval (Register inp a) = handleRegistration inp $> a
      eval (Authenticate inp a) = handleAuthentication inp $> a
      eval (UserLogout a) = do
          modify (stCurrentUser .~ Nothing)
          modify (stAuthToken .~ Nothing)
          eval (Goto Home unit)
          pure a

      handleNewSession (Edit fn) = modify (stCurrentSession %~ fn)
      handleNewSession Submit = do
          auth <- gets (\s -> Tuple <$> s.authToken <*> s.currentUser)
          for_ auth \(Tuple token user) -> do
              sess <- gets _.currentSession
              result <- liftAff' (API.postSession token user sess)
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
                                    }
                   modify (stCurrentUser ?~ saved)
                   eval (Goto Profile unit)
               Left err -> do
                   modify (stErrors ?~ ["There was an error registering:", err])

      handleAuthentication (Edit fn) = modify (stAuthentication %~ fn)
      handleAuthentication Submit = do
          auth <- gets _.authentication
          res <- liftAff' (API.postAuthentication auth)
          case res of
               Nothing ->
                   modify (stErrors ?~ ["That login wasn't quite right. Try again?"])
               Just (Tuple session user) -> do
                   liftEff' (WS.setItem WS.localStorage "auth" session)
                   modify (stErrors ?~ [])
                   modify (stCurrentUser ?~ user)
                   modify (stAuthToken ?~ session)
                   eval (Goto Profile unit)
