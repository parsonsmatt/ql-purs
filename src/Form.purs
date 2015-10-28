module Form where

import BigPrelude
import Unsafe.Coerce

import Data.Tuple
import Data.Array
import Data.Either
import Data.Maybe

import Halogen (action)
import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed (IProp(..))
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Forms as EF
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.Indexed as H

import Halogen.HTML.Core (Prop(..), HTML(..))

submitButton t h =
  H.button [ E.onClick (\_ -> E.preventDefault $> E.stopPropagation $> action h), P.classes [ B.btn, B.btnPrimary ] ]
    [ H.text t ]
  
submitButton_ = submitButton "Submit"

form onSubmit fields =
  H.form_
    [ H.div [ P.classes [ B.formGroup ] ] 
      (snoc fields (submitButton_ onSubmit)) 
    ]

date id_ label value onChange =
  H.div [ P.classes [ B.formGroup ] ]
    [ H.label [ P.for id_ ] [ H.text label ]
    , H.input 
      [ P.id_ id_
      , P.classes [ B.formControl ]
      , P.inputType P.InputDate
      , P.value value 
      , E.onValueChange (E.input onChange)
      ]
    ]

textarea id_ label value onChange = 
  H.div [ P.classes [ B.formGroup ] ] 
  [ H.label [ P.for id_ ]
    [ H.text label ]
  , H.textarea 
    [ P.id_ id_
    , P.class_ B.formControl
    , P.value value 
    , E.onValueChange (E.input onChange)
    ]
  ]
