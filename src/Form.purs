module Form where

import BigPrelude
import Unsafe.Coerce

import Data.Date
import Data.Date.UTC
import Data.Tuple
import Data.Array
import Data.Either
import Data.Maybe

import Halogen (action)
import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed (IProp(..))
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.Indexed as H

import Halogen.HTML.Core (Prop(..), HTML(..))

-- Ported from yesod-form

data FormResult a
  = FormMissing
  | FormFailure (Array String)
  | FormSuccess a

instance functorFormResult :: Functor FormResult where
  map _ FormMissing =
    FormMissing
  map _ (FormFailure errs) = 
    FormFailure errs
  map f (FormSuccess a) =
    FormSuccess (f a)

instance applyFormResult :: Apply FormResult where
  apply (FormSuccess f) (FormSuccess a) =
    FormSuccess (f a)
  apply (FormFailure errs) (FormFailure errs') =
    FormFailure (errs <> errs')
  apply (FormFailure e) _ =
    FormFailure e
  apply _ (FormFailure e) =
    FormFailure e
  apply _ _ = FormMissing

instance applicativeFormResult :: Applicative FormResult where
  pure = FormSuccess

instance bindFormResult :: Bind FormResult where
  bind FormMissing _ = FormMissing
  bind (FormFailure errs) _ = FormFailure errs
  bind (FormSuccess a) f = f a

instance semigroupFormResult :: (Semigroup a) => Semigroup (FormResult a) where
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
  , fieldView: textFieldView
  }

form handler xs =
  H.form 
    [ P.classes [ B.formGroup ]
    , E.onSubmit (\e -> E.preventDefault *> E.stopPropagation $> action (handler e))
    ]
    (concat [xs, [submitButton_]])

textFieldView :: FieldViewFn String
textFieldView = fieldView P.InputText id

submitButton t =
  H.button [ P.classes [ B.btn, B.btnPrimary ] ]
    [ H.text t ]
  
submitButton_ = submitButton "Submit"

textAreaFieldView :: FieldViewFn String
textAreaFieldView ident name attrs val isReq =
  H.div [ P.classes [ B.formGroup ] ] 
    [ H.label [ P.class_ B.controlLabel, P.for ident ]
      [ H.text name ]
    , H.textarea attributes
    ]
  where
    attributes = unsafeCoerce attrs <>
                   [ P.classes [ B.formControl ]
                   , P.value (either id id val)
                   , P.id_ ident
                   , P.name name
                   , P.required isReq
                   ]


dateFieldView :: FieldViewFn Date
dateFieldView = fieldView P.InputDate formDateFromDate

formDateFromDate :: Date -> String
formDateFromDate date =
  let y = intFromYear $ year date
      m = 1 + (monthFromEnum $ month date)
      d = intFromDayOfMonth (dayOfMonth date)
   in show y ++ "-" ++ show m ++ "-" ++ show d

intFromDayOfMonth :: DayOfMonth -> Int
intFromDayOfMonth (DayOfMonth d) = d

monthFromEnum :: Month -> Int
monthFromEnum January   = 0
monthFromEnum February  = 1
monthFromEnum March     = 2
monthFromEnum April     = 3
monthFromEnum May       = 4
monthFromEnum June      = 5
monthFromEnum July      = 6
monthFromEnum August    = 7
monthFromEnum September = 8
monthFromEnum October   = 9
monthFromEnum November  = 10
monthFromEnum December  = 11

intFromYear :: Year -> Int
intFromYear (Year i) = i

fieldView :: forall a. P.InputType -> (a -> String) -> FieldViewFn a
fieldView inpType shower ident name attrs val isReq =
  H.div [ P.classes [ B.formGroup ] ]
    [ H.label [ P.class_ B.controlLabel, P.for ident ]
      [ H.text name ]
    , H.input attributes
    ]
  where
    attributes = unsafeCoerce attrs <>
                   [ P.classes [ B.formControl ]
                   , P.value (either id shower val)
                   , P.inputType inpType
                   , P.id_ ident
                   , P.name name
                   , P.required isReq
                   ]


newtype Form a = Form (String -> Tuple a String)

runForm :: forall a. Form a -> String -> Tuple a String
runForm (Form a) = a

first f (Tuple a b) = Tuple (f a) b

instance functorForm :: Functor Form where
  map f (Form fn) = Form (first f <<< fn)
