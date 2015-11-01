module Form where

import BigPrelude
import Unsafe.Coerce
import qualified Data.String as Str

import Optic.Lens
import Optic.Core

import Data.Tuple
import Data.Array hiding ((..))
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

data FormInput a
  = Submit
  | Edit (a -> a)


submitButton t h =
  H.button [ E.onClick (\_ -> E.preventDefault $> E.stopPropagation $> action h), P.classes [ B.btn, B.btnPrimary ] ]
    [ H.text t ]
  
submitButton_ = submitButton "Submit"

lensyForm data_ eventType fields = 
  form (eventType Submit) (map (\f -> f data_ eventType) fields)

lensyPassword = lensyValidatingField P.InputPassword

lensyEmail 
  :: forall a f v
   . String
  -> String
  -> LensP a String 
  -> a 
  -> (FormInput a -> Unit -> f Unit) 
  -> HTML v (f Unit)
lensyEmail id_ label lens_ = lensyValidatingField P.InputEmail id_ label lens_ validEmail
  where
    validEmail str = maybe (Left "Must have @ symbol") (const (Right str)) (Str.indexOf "@" str)

lensyValidatingField
  :: forall a f v.
     P.InputType
  -> String
  -> String
  -> LensP a String
  -> (String -> Either String String)
  -> a
  -> (FormInput a -> Unit -> f Unit)
  -> HTML v (f Unit)
lensyValidatingField type_ id_ label lens_ validator data_ eventType = 
  H.div [ P.classes classes ]
    [ H.label [ P.for id_ ] [ H.text label ]
    , H.input 
      [ P.id_ id_
      , P.classes [ B.formControl ]
      , P.inputType type_
      , P.value item 
      , E.onValueChange (E.input (eventType .. Edit .. set lens_))
      ]
    , H.span_ [ H.text errMsg ]
    ]
  where
    validation = validator item
    item = data_ ^. lens_ 
    classes = case validation of
                   Left _ -> [ B.formGroup, B.hasError ]
                   Right _ -> [ B.formGroup ]
    errMsg = case validation of
                  Left str -> str
                  Right _ -> ""

lensyField
  :: forall a f v.
     P.InputType
  -> String
  -> String
  -> LensP a String
  -> a
  -> (FormInput a -> Unit -> f Unit)
  -> HTML v (f Unit)
lensyField type_ id_ label lens_ data_ eventType = lensyValidatingField type_ id_ label lens_ Right data_ eventType

form onSubmit fields =
  H.form_
    [ H.div [ P.classes [ B.formGroup ] ] 
      (snoc fields (submitButton_ onSubmit)) 
    ]

text = field P.InputText

email = field P.InputEmail

date = field P.InputDate

password = field P.InputPassword

field type_ id_ label value onChange = 
  H.div [ P.classes [ B.formGroup ] ]
    [ H.label [ P.for id_ ] [ H.text label ]
    , H.input 
      [ P.id_ id_
      , P.classes [ B.formControl ]
      , P.inputType type_
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
