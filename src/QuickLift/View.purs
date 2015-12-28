module QuickLift.View where

import BigPrelude

import qualified Data.String as Str
import Data.Array hiding ((..))

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.Themes.Bootstrap3 as B

import qualified Form as F
import qualified Form.WForm as WF
import Form.Types (FormInput(..))

import QuickLift.State
import QuickLift.Model
import QuickLift.Input
import qualified QuickLift.Api as API

import qualified Layout as L

import Types
import Types.Date

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
    H.div_ $ errs st.errors :
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
  linkTo (Sessions </> Show n) "asdfffff"

errs :: forall a b. Maybe (Array String) -> HTML a b
errs Nothing = H.div_ []
errs (Just errors) = H.div_ (map (\e -> H.p_ [H.text e]) errors)

linkSession :: forall a. Session -> HTML a Input
linkSession (Session s) =
  linkTo (Sessions </> Show s.id) (yyyy_mm_dd s.date)


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
  , linkTo (Sessions Index) "Go to sessions"
  ]
