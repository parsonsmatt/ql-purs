module Form.Types where

import Data.Functor (($>))

import Halogen (action)
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B

data FormInput a
  = Submit
  | Edit (a -> a)

submitButton t h =
  H.button [ E.onClick (\_ -> E.preventDefault $> E.stopPropagation $> action h), P.classes [ B.btn, B.btnPrimary ] ]
    [ H.text t ]
  
submitButton_ = submitButton "Submit"
