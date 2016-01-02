module QuickLift.Api.Util where

import Prelude

import Control.Alt
import Data.Either
import Data.Foreign
import Data.Foreign.Class

-- | `foreignToEither` converts an object corresponding to an Aeson encoded
-- | Either value, with a structure like:
-- |
-- | ```
-- | { Right: "the value" }
-- | { Left: 12345 }
-- | ```
foreignToEither
    :: forall e a
     . (IsForeign a, IsForeign e)
    => Foreign
    -> F (Either e a)
foreignToEither fgn = Right <$> readProp "Right" fgn <|> Left <$> readProp "Left" fgn

-- | `joinForeign` takes a function which converts a `ForeignError` into an
-- | value of type `e` and collapses the two layers into a single Either.
joinForeign
    :: forall e a
     . (IsForeign a, IsForeign e)
     => (ForeignError -> e)
     -> Foreign
     -> Either e a
joinForeign f = either (Left <<< f) id <<< foreignToEither

-- | `readEither` will attempt to read a value belonging to one of two types.
-- | It is right biased, allowing for use of `Either String SomeType` error
-- | reporting.
readEither
    :: forall e a
     . (IsForeign a, IsForeign e)
    => Foreign
    -> F (Either e a)
readEither f = Right <$> read f <|> Left <$> read f
