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
  -> String
  -> String
  -> LensP a String
  -> (String -> Either String String)
  -> WForm v f a
field inpType id_ label lens_ validator = do
    Tuple data_ eventType <- ask
    let item = data_ ^. lens_
        validation = validator item
        classes = case validation of
                       Left _ -> [ B.formGroup, B.hasError ]
                       Right _ -> [ B.formGroup ]
        errMsg = case validation of
                      Left str -> str
                      Right _ -> ""
    tell [html data_ eventType errMsg classes item]
    -- TODO: handle this better
    return (unsafeCoerce item)
  where
    html :: a -> FormAction f a -> String -> _ -> String -> HTML v (f Unit)
    html data_ eventType errMsg classes item = 
      H.div [ P.classes classes ]
        [ H.label [ P.for id_ ] [ H.text label ]
        , H.input 
          [ P.id_ id_
          , P.classes [ B.formControl ]
          , P.inputType inpType
          , P.value item 
          , E.onValueChange (E.input (eventType .. Edit .. set lens_))
          ]
        , H.span_ [ H.text errMsg ]
        ]
