module Types.Date where

import BigPrelude

import qualified Data.String as Str
import Data.Foreign
import Data.Foreign.Class
import Data.Function

import qualified Data.Date as D
import qualified Data.Date.UTC as U
import qualified Data.Time as T

newtype Date = Date D.Date

instance eqDate :: Eq Date where
  eq = eq `on` underlyingDate

instance ordDate :: Ord Date where
  compare = compare `on` underlyingDate

underlyingDate :: Date -> D.Date
underlyingDate (Date d) = d

now :: forall e. Eff (now :: D.Now | e) Date
now = Date <$> D.now

dateFromString :: String -> Maybe Date
dateFromString str = Date <$> D.fromString str

year :: Date -> D.Year
year = U.year <<< underlyingDate

month :: Date -> D.Month
month = U.month <<< underlyingDate

dayOfMonth :: Date -> D.DayOfMonth
dayOfMonth = U.dayOfMonth <<< underlyingDate

unYear :: D.Year -> Int
unYear (D.Year n) = n

unDayOfMonth :: D.DayOfMonth -> Int
unDayOfMonth (D.DayOfMonth n) = n

instance isForeignDate :: IsForeign Date where
  read = readDate

readDate :: Foreign -> F Date
readDate f =
  case tagOf f of
       "Date" ->
         case D.fromJSDate <$> unsafeReadTagged "Date" f of
              Right (Just d) -> Right (Date d)
              Right Nothing -> Left (TypeMismatch "invalid date" "asdf")
              Left a -> Left a
       "String" ->
         case D.fromString <$> unsafeReadTagged "String" f of
              Right (Just d) -> Right (Date d)
              Right Nothing -> Left (TypeMismatch "invalid date" "invalid date")
              Left a -> Left a
       "Number" ->
         case D.fromEpochMilliseconds <<< T.Milliseconds <$> unsafeReadTagged "Number" f of
              Right (Just d) -> (Right (Date d))
              Right Nothing -> Left (TypeMismatch "invalid read" "expecting epoch milliseconds")
              Left a -> Left a
       _ ->
         Left (TypeMismatch "Expecting date" (tagOf f))

toISOString :: Date -> String
toISOString (Date d) = runFn2 jsDateMethod "toISOString" (D.toJSDate d)

foreign import jsDateMethod :: forall a. Fn2 String D.JSDate a

yyyy_mm_dd :: Date -> String
yyyy_mm_dd (Date date) = y ++ "-" ++ m ++ "-" ++ d
  where
    y = (ypad <<< show) case U.year date of D.Year n -> n
    m = pad (1 + (fromEnum $ U.month date))
    d = pad case U.dayOfMonth date of D.DayOfMonth day -> day
    pad n = let str = show n
             in case Str.length str of
                   1 ->  "0" ++ str
                   _ -> str
    ypad str =
      case Str.length str of
           0 -> "0000"
           1 -> "000" ++ str
           2 -> "00" ++ str
           3 -> "0" ++ str
           _ -> str
