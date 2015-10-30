module QuickLift where

import BigPrelude

import Data.Foldable

import Data.Int hiding (fromString)
import Data.Functor.Coproduct (Coproduct(..), left)
import Control.Monad
import Data.Array
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
import QuickLift.Model
import qualified QuickLift.Api as API
import qualified Layout as L

import Types
import Types.Date

data Input a 
  = Goto Routes a
  | GetUser Int a
  | LoadSessions a
  | NewSession NewSessionInput a

data NewSessionInput
  = Submit
  | EditDate String
  | EditText String

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
  }

type State =
  { currentPage :: Routes
  , currentUser :: Maybe User
  , loadedSessions :: Array Session
  , currentSession :: Session
  }

ui :: forall eff. Component State Input (QLEff eff)
ui = component render eval
  where
    render state =
      L.defaultLayout
        [ view state.currentPage state
        ]

    eval :: Eval Input State Input (QLEff eff)
    eval (Goto route next) = do
      modify (_ { currentPage = route })
      case route of
           Sessions Index -> eval (LoadSessions unit)
           _ -> pure unit
      pure next

    eval (GetUser i n) = do
      newUser <- liftAff' $ API.getUser i
      modify (_{ currentUser = newUser })
      pure n

    eval (LoadSessions a) = do
      s <- liftAff' (API.getUserSessions 1)
      modify (_{ loadedSessions = join $ maybeToArray s })
      pure a

    eval (NewSession inp a) = do
      handleNewSession inp
      pure a

    handleNewSession Submit = do
      { currentSession: Session st, loadedSessions: ss } <- get
      result <- liftAff' (API.postSession (Session st))
      for_ result \n -> do
        let saved = Session (st { id = n })
            rt = Sessions </> Show n
        modify (_{ currentSession = saved
                 , loadedSessions = saved : ss
                 })
        eval (Goto (Sessions </> Show n) unit)
        liftAff' (updateUrl rt)

    handleNewSession (EditDate str) = do
      Session s <- gets _.currentSession
      let d = fromMaybe s.date (dateFromString str)
      when (s.date /= d) do
        modify (_ { currentSession = Session (s { date = d })})

    handleNewSession (EditText str) = do
      Session s <- gets _.currentSession
      modify (_ { currentSession = Session (s { text = str })})

view :: Routes -> State -> ComponentHTML Input
view Home _ = 
  H.div_
  [ H.h1_ [ H.text "QuickLift" ]
  , H.p_ [ H.text "Welcome to QuickLift" ]
  ]

view Profile st =
  H.div_
    [ H.h1_ [ H.text "Home" ]
    , H.p_ [ H.text "what a nice profile!" ]
    , H.div_ (printUser st.currentUser)
    , H.a [ E.onClick $ E.input_ (GetUser 1) ] 
      [ H.text "Login (lol)" ]
    ]

view (Sessions Index) st =
  let sessions = case map linkSession st.loadedSessions of
                      [] -> H.p_ [ H.text "No sessions." ]
                      xs -> H.ul_ (map (H.li_ <<< pure) xs)
   in H.div_
    [ newButton
    , loadButton
    , sessions
    ]

view (Sessions (Show n)) st =
  let maybeIndex = findIndex (\(Session s) -> s.id == n) st.loadedSessions 
      session = maybeIndex >>= \i -> st.loadedSessions !! i
   in showPage n session

view (Sessions New) st =
  H.div_ 
    [ F.form (NewSession Submit)
      [ F.textarea "session" "Session:" (getSessionText $ st.currentSession) (NewSession <<< EditText)
      , F.date "date" "Date:" (yyyy_mm_dd <<< getSessionDate $ st.currentSession) (NewSession <<< EditDate)
      ]
    ]

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
