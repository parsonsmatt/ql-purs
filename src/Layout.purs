module Layout where

import BigPrelude (Maybe(Just, Nothing), map)

import Data.Array ((:))

import Halogen.HTML.Indexed (HTML(), ClassName())
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import QuickLift.State (State)
import QuickLift.Model (User)
import Types (CRUD(Index, New), Routes(Logout, Registration, Login, Profile, Sessions, Home), linkTo, (</>), link)

row :: forall a b. Array (HTML a b) -> HTML a b
row = H.div [ P.class_ B.row ]

col :: forall a b. ClassName -> Array (HTML a b) -> HTML a b
col sz = H.div [ P.class_ sz ]

col' :: forall a b. Array ClassName -> Array (HTML a b) -> HTML a b
col' szs = H.div [ P.classes szs ]

defaultLayout :: State -> Array (HTML _ _) -> HTML _ _
defaultLayout st page =
  H.div [ P.class_ B.container ]
    [ header st.currentUser
    , row
        [ col' [ B.colLg10, B.colLgOffset1 ] page ]
   -- , row
   --   [ col' [ B.colMd8, B.colMdOffset2 ]
   --     [ footer ]
   --   ]
    ]

container attrs = H.div (P.class_ B.container : attrs)

container_ = container []

header :: Maybe User -> HTML _ _
header muser =
  H.nav [ P.classes [ B.navbarNav, B.navbarFixedTop, B.navbarInverse] ]
    [ container_
      [ H.a [ P.classes [ B.navbarBrand ], P.href (link Home) ]
        [ H.text "QuickLift" ]
      , H.ul [ P.classes [ B.navbarNav, B.nav, B.navTabs] ]
        [ H.li_ [ linkTo (Sessions </> New) "Log" ]
        , H.li_ [ linkTo (Sessions </> Index) "Sessions" ]
        , H.li_ [ linkTo Profile "Profile" ]
        ]
      , case muser of
             Nothing ->
                 H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
                     [ H.li_ [ linkTo Login "Log in" ]
                     , H.li_ [ linkTo Registration "Sign up" ]
                     ]
             Just u ->
                 H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
                     [ H.li_ [ linkTo Logout "Log out" ] ]

      ]
    ]

footer :: forall a b. HTML a b
footer =
  H.footer [ P.class_ (H.className "footer") ]
    [ H.text "QuickLift"
    , H.ul []
      (map (\s -> H.li [] [ H.text s ]) [ "About", "Contact" ] )
    ]
