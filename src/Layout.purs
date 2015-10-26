module Layout where

import BigPrelude

import Data.Array

import Halogen.HTML.Indexed (HTML(), ClassName())
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.Events.Indexed as E

import Types
import HasLink


row :: forall a b. Array (HTML a b) -> HTML a b
row = H.div [ P.class_ B.row ]

col :: forall a b. ClassName -> Array (HTML a b) -> HTML a b
col sz = H.div [ P.class_ sz ]

col' szs = H.div [ P.classes szs ]

--defaultLayout :: forall a b. Array (HTML a b) -> HTML a b
defaultLayout page =
  H.div [ P.class_ B.container ]
    [ header
    , row
        [ col' [ B.colLg10, B.colLgOffset1 ] page ]
    , row
      [ col' [ B.colMd8, B.colMdOffset2 ]
        [ footer ]
      ]
    ]

--container :: forall a b. _ -> Array (HTML a b) -> HTML a b
container attrs = H.div (P.class_ B.container : attrs)

--container_ :: forall a b. Array (HTML a b) -> HTML a b
container_ = container []

header =
  H.nav [ P.classes [ B.navbarNav, B.navbarFixedTop, B.navbarInverse] ]
    [ container_
      [ H.a [ P.classes [ B.navbarBrand ], P.href (link Home) ] 
        [ H.text "QuickLift" ]
      , H.ul [ P.classes [ B.navbarNav, B.nav, B.navTabs] ]
        [ H.li_ [ H.a [ P.href (link (Sessions New)) ]
                [ H.text "Log a Session" ] ]
        , H.li_ [ H.a [ P.href (link Profile) ] 
                [ H.text "See your Profile" ] ]
        ]
      , H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
        [ H.li_ 
          [ H.a [ P.href "#" ] 
                [ H.text "Log in" ] 
          ]
        , H.li_ 
          [ H.a [ P.href "#" ]
                [ H.text "Sign up" ]
          ]
        ]
      ]
    ]

footer :: forall a b. HTML a b
footer =
  H.footer [ P.class_ (H.className "footer") ]
    [ H.text "QuickLift"
    , H.ul []
      (map (\s -> H.li [] [ H.text s ] ) [ "About", "Contact", "Facebook", "Twitter" ] )
    ]
