module Form.AForm where

import Prelude
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
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

import Form.Types

type AForm v f a =
  ReaderT
    (Tuple a (FormAction f a))
    (Writer FormError)
    (Array (HTML v (f Unit)) -> Array (HTML v (f Unit)))

type FormError = Array String
type FormAction f a = FormInput a -> Unit -> f Unit

renderForm 
  :: forall a v f
   . a 
  -> FormAction f a
  -> AForm v f a
  -> Array (HTML v (f Unit))
renderForm _data eventType fields =
  (fst (runWriter (runReaderT fields (Tuple _data eventType)))) [submitButton_ (eventType Submit)]

getErrors :: forall v f a. a -> FormAction f a -> AForm v f a -> FormError
getErrors a f w = execWriter (runReaderT w (Tuple a f))

field
  :: forall a f v
   . P.InputType
  -> String
  -> String
  -> LensP a String
  -> (String -> Either String String)
  -> AForm v f a
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
    when (errMsg /= "") (tell [errMsg])
    return (\arr -> cons (html data_ eventType errMsg classes item) arr)
  where
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
