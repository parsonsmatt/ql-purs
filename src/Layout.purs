module Layout where

import Prelude

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.Themes.Bootstrap3 as B

row = H.div [ P.class_ B.row ]
col sz = H.div [ P.class_ sz ]

container page =
  H.div [ P.class_ B.container ]
    [ H.div [ P.class_ B.row ]
      [ col B.colLg12
        [ header ]
      ]
    , row
        [ col B.colLg8 page ]
    , row
      [ col B.colLg12
        [ footer ]
      ]
    ]

header =
  H.header [ P.class_ B.navbarHeader ]
    [ H.h1_ [ H.text "QuickLift" ]
    ]

footer =
  H.footer [ P.class_ (H.className "footer") ]
    [ H.text "QuickLift is a thing I guess"
    ]
