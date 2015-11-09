module Form.WForm where

import Prelude
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import qualified Data.String as Str

import Unsafe.Coerce

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

import Form.Types

type WForm v f a =
  ReaderT
    (Tuple a (FormAction f a))
    -- TODO: Tuple of arrays of errors and HTMLs
    (Writer (Array (HTML v (f Unit))))
    a

type FormError = Array String
type FormAction f a = FormInput a -> Unit -> f Unit

renderForm
  :: forall a v f
   . a
  -> FormAction f a
  -> WForm v f a
  -> Array (HTML v (f Unit))
renderForm _data eventType fields =
  execWriter (runReaderT fields (Tuple _data eventType)) ++ [submitButton_ (eventType Submit)]

emailField = field P.InputEmail
textField = field P.InputText
passwordField = field P.InputPassword

field
  :: forall a b f v
   . P.InputType
  -> String -- ID
  -> String -- label
  -> LensP source a -- lens into containing data structure
  -> (a -> String) -- rendering data into string
  -> (String -> Either String a) -- validation function
  -> WForm v f a
field inpType id_ label lens_ render validator = do
    Tuple data_ eventType <- ask
    let item = data_ ^. lens_
        validation = validator item
        classes = case validation of
                       Left _ -> [ B.formGroup, B.hasError ]
                       Right _ -> [ B.formGroup ]
        errMsg = case validation of
                      Left str -> str
                      Right _ -> ""
        html =
          H.div [ P.classes classes ]
            [ H.label [ P.for id_ ] [ H.text label ]
            , H.input
              [ P.id_ id_
              , P.classes [ B.formControl ]
              , P.inputType inpType
              , P.value (render item)
              , E.onValueChange (E.input (eventType .. Edit .. \str -> either id (set lens_ str) (validator str)))
              ]
            , H.span_ [ H.text errMsg ]
            ]
    tell [html]
    -- TODO: handle this better
    return (unsafeCoerce item)
