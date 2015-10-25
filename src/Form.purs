module Form where

import Prelude
import Unsafe.Coerce

import Data.Array
import Data.Either
import Data.Maybe

import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed (IProp(..))
import qualified Halogen.HTML.Indexed as H

import Halogen.HTML.Core (Prop(..), HTML(..))

-- Ported from yesod-form

data Form a
  = FormMissing
  | FormFailure (Array String)
  | FormSuccess a

instance functorForm :: Functor Form where
  map _ FormMissing =
    FormMissing
  map _ (FormFailure errs) = 
    FormFailure errs
  map f (FormSuccess a) =
    FormSuccess (f a)

instance applyForm :: Apply Form where
  apply (FormSuccess f) (FormSuccess a) =
    FormSuccess (f a)
  apply (FormFailure errs) (FormFailure errs') =
    FormFailure (errs <> errs')
  apply (FormFailure e) _ =
    FormFailure e
  apply _ (FormFailure e) =
    FormFailure e
  apply _ _ = FormMissing

instance applicativeForm :: Applicative Form where
  pure = FormSuccess

instance semigroupForm :: (Semigroup a) => Semigroup (Form a) where
  append a b = append <$> a <*> b

type FieldViewFn a
   = forall r p i
   . String -- id
  -> String -- name
  -> Array (IProp r i) -- attrs
  -> Either String a -- Either (invalid text) (parsed result)
  -> Boolean -- required?
  -> HTML p i

newtype Field a
  = Field
  { fieldParse :: Array String -> Either String (Maybe a)
  , fieldView :: FieldViewFn a
  }

type FormMessage = String

parseHelper :: forall a. (String -> Either FormMessage a) -> Array String -> Either FormMessage (Maybe a)
parseHelper f xs =
  case head xs of
       Nothing -> Right Nothing
       Just "" -> Right Nothing
       Just b -> map Just (f b)

textField :: Field String
textField = Field
  { fieldParse: parseHelper Right
  , fieldView: \ident name attrs val isReq -> 
      H.input (unsafeCoerce attrs <> [ P.value (either id id val)
                        , P.inputType P.InputText
                        , P.id_ ident
                        , P.name name
                        ]
              )
  }
