module QuickLift where

import BigPrelude

import Data.Foldable
import qualified Data.String as Str

import Optic.Lens
import Optic.Core

import Data.Int hiding (fromString)
import Data.Functor.Coproduct (Coproduct(..), left)
import Control.Monad
import Data.Array hiding ((..))
import Control.Monad.Eff.Console
import Control.Monad.Eff.Unsafe
import qualified Routing.Hash.Aff as R

import Halogen
import qualified Halogen.Extra as EX
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.Themes.Bootstrap3 as B

import qualified Form as F
import qualified Form.AForm as AF
import qualified Form.WForm as WF
import Form.Types (FormInput(..))
import QuickLift.Model
import qualified QuickLift.Api as API
import qualified Layout as L

import Types
import Types.Date

data Input a 
  = Goto Routes a
  | GetUser Int a
  | LoadSessions a
  | NewSession (FormInput Session) a
  | Register (FormInput UserReg) a
  | Authenticate (FormInput UserAuth) a

type State =
  { currentPage :: Routes
  , currentUser :: Maybe User
  , loadedSessions :: Array Session
  , currentSession :: Session
  , registration :: UserReg
  , authentication :: UserAuth
  }

initialState :: State
initialState =
  { currentPage: Home
  , currentUser: Nothing
  , loadedSessions: []
  , currentSession: Session
    { date: runPure (unsafeInterleaveEff now)
    , text: ""
    , userId: 1
    , id: -1
    }
  , registration: emptyReg
  , authentication: emptyAuth
  }

stRegistration :: LensP State UserReg
stRegistration =
  lens
    (_.registration)
    (_ { registration = _ })

stCurrentSession :: LensP State Session
stCurrentSession =
  lens
    (_.currentSession)
    (_ { currentSession = _ })

stLoadedSessions :: LensP State (Array Session)
stLoadedSessions =
  lens 
    (_.loadedSessions)
    (_ { loadedSessions = _ })

stCurrentUser :: LensP State (Maybe User)
stCurrentUser =
  lens
    (_.currentUser)
    (_ { currentUser = _ })

stAuthentication :: LensP State UserAuth
stAuthentication = lens _.authentication _ { authentication = _ }

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


renderView :: Routes -> State -> ComponentHTML Input
renderView Home _ = 
  H.div_
  [ H.h1_ [ H.text "QuickLift" ]
  , H.p_ [ H.text "Welcome to QuickLift" ]
  ]


renderView Profile st =
  H.div_
    [ H.h1_ [ H.text "Home" ]
    , H.p_ [ H.text "what a nice profile!" ]
    , H.div_ (printUser st.currentUser)
    ]


renderView (Sessions Index) st =
  let sessions = case map linkSession st.loadedSessions of
                      [] -> H.p_ [ H.text "No sessions." ]
                      xs -> H.ul_ (map (H.li_ <<< pure) xs)
   in H.div_
    [ newButton
    , loadButton
    , sessions
    ]


renderView (Sessions (Show n)) st =
  let maybeIndex = findIndex (eq n .. view (_Session .. id_)) st.loadedSessions 
      session = maybeIndex >>= index (st ^. stLoadedSessions)
   in showPage n session


renderView (Sessions New) st =
  H.div_ 
    [ F.form (NewSession Submit)
      [ F.textarea "session" "Session:" 
        (st.currentSession ^. _Session .. text_)
        (NewSession .. Edit .. set (_Session .. text_))
      , F.date "date" "Date:" 
        (yyyy_mm_dd (st.currentSession ^. _Session .. date_)) 
        (NewSession .. Edit .. edDate)
      ]
    ]
  where
    edDate :: String -> Session -> Session
    edDate str sess =
      let d = fromMaybe (sess ^. _Session .. date_) (dateFromString str)
       in sess # _Session .. date_ .~ d 


renderView Registration st = 
  H.div_ $
    WF.renderForm st.registration Register do
      WF.textField "name" "Name:" (_UserReg .. name) Right
      WF.emailField "email" "Email:" (_UserReg .. email) validEmail
      WF.passwordField "password" "Password:" (_UserReg .. password) validPassword
      WF.passwordField "confirm" "Confirmation:" (_UserReg .. confirmation) validConfirmation
  where
    validPassword str
      | Str.length str < 6 = Left "Password must be at least 6 characters"
      | otherwise = Right str
    validConfirmation str
      | str == st ^. stRegistration .. _UserReg .. password = Right str
      | otherwise = Left "Password must match confirmation"
    validEmail str = maybe (Left "Must have @ symbol") (const (Right str)) (Str.indexOf "@" str)

renderView Login st = 
  H.div_ $
    WF.renderForm st.authentication Authenticate do
      WF.emailField "email" "Email:" (_UserAuth .. email) validEmail
      WF.passwordField "password" "Password:" (_UserAuth .. password) validPassword
      WF.passwordField "confirm" "Confirmation:" (_UserAuth .. confirmation) validConfirmation
  where
    validPassword str
      | Str.length str < 6 = Left "Password must be at least 6 characters"
      | otherwise = Right str
    validConfirmation str
      | str == st ^. stAuthentication .. _UserAuth .. password = Right str
      | otherwise = Left "Password must match confirmation"
    validEmail str = maybe (Left "Must have @ symbol") (const (Right str)) (Str.indexOf "@" str)

succLink :: forall a. Maybe Int -> HTML a Input
succLink Nothing =
  H.p_ [ H.text "lol" ]
succLink (Just n) =
  H.a [ P.href (link (Sessions $ Show n)) ]
    [ H.text "asdfffff" ]


linkSession :: forall a. Session -> HTML a Input
linkSession (Session s) =
  H.a [ P.href (link (Sessions </> Show s.id)) ]
    [ H.text (yyyy_mm_dd s.date) ]


loadButton :: forall a. HTML a Input
loadButton =
  H.button [ P.classes [ B.btn, B.btnSuccess ], E.onClick $ E.input_ LoadSessions ]
    [ H.text "Loaaaad" ]

newButton :: forall a. HTML a Input
newButton = 
  H.p_
    [ H.a [ P.href (link (Sessions </> New)), P.classes [B.btn, B.btnDefault] ]
      [ H.text "New Session" ]
    ]

showPage :: forall a. Int -> Maybe Session -> HTML a Input
showPage n (Just (Session s)) =
  H.div_ 
    [ H.h1_ [ H.text $ yyyy_mm_dd s.date ]
    , H.p_ [ H.text s.text ]
    , newButton
    ]
showPage n Nothing = 
  H.div_
    [ H.h2_ [ H.text "hmm, not found... load it?" ]
    , loadButton
    ]


printUser :: forall a b. Maybe User -> Array (HTML a b)
printUser Nothing = []
printUser (Just (User user)) =
  [ H.p_ [ H.text ("Hello, " <> user.name <> "!") ]
  , H.a [ P.href (link $ Sessions Index) ]
    [ H.text "Go to sessions" ]
  ]
