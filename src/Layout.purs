module Layout where

import BigPrelude

import Data.Array

import Halogen.HTML.Indexed (HTML(), ClassName())
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.Events.Indexed as E

row :: forall a b. Array (HTML a b) -> HTML a b
row = H.div [ P.class_ B.row ]

col :: forall a b. ClassName -> Array (HTML a b) -> HTML a b
col sz = H.div [ P.class_ sz ]

--defaultLayout :: forall a b. Array (HTML a b) -> HTML a b
defaultLayout page =
  H.div [ P.class_ B.container ]
    [ header
    , row
        [ col B.colLg8 page ]
    , row
      [ col B.colLg12
        [ footer ]
      ]
    ]

--container :: forall a b. _ -> Array (HTML a b) -> HTML a b
container attrs = H.div (P.class_ B.container : attrs)

--container_ :: forall a b. Array (HTML a b) -> HTML a b
container_ = container []

header =
  H.nav [ P.classes [ B.navbarNav, B.navbarFixedTop, B.navbarDefault] ]
    [ container_
      [ H.a [ P.classes [ B.navbarBrand ], P.href "#" ] 
        [ H.text "QuickLift" ]
      , H.ul [ P.classes [ B.navbarNav, B.nav ] ]
        [ H.li_ [ H.a [ P.href "#session" ]
                [ H.text "Log a Session" ] ]
        , H.li_ [ H.a [ P.href "#profile" ] 
                [ H.text "See your Profile" ] ]
        ]
      ]
    ]

footer :: forall a b. HTML a b
footer =
  H.footer [ P.class_ (H.className "footer") ]
    [ H.text "QuickLift is a thing I guess"
    ]
