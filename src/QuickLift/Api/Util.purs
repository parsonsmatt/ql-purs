module QuickLift.Api.Util where

import Prelude

import Control.Monad.Aff
import Control.Alt
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Foreign hiding (isNull, isArray)
import Data.Foreign.Class
import Network.HTTP.Affjax
import Network.HTTP.Affjax as AJ
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.RequestHeader

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
    :: forall e a. (IsForeign a, IsForeign e)
    => (ForeignError -> e)
    -> Foreign
    -> Either e a
joinForeign f = either (Left <<< f) id <<< foreignToEither


readTuple
    :: forall a b. (IsForeign a, IsForeign b)
    => String
    -> String
    -> Foreign
    -> F (Tuple a b)
readTuple a b r = Tuple <$> readProp a r <*> readProp b r

-- | `readEither` will attempt to read a value belonging to one of two types.
-- | It is right biased, allowing for use of `Either String SomeType` error
-- | reporting.
readEither
    :: forall e a
     . (IsForeign a, IsForeign e)
    => Foreign
    -> F (Either e a)
readEither f = Right <$> read f <|> Left <$> read f

-- | This method wraps the Affjax defaultRequest and specializes it for the
-- | QuickLift application. Other functions in the file build on top of the
-- | functionality presented here.
qlReqHeaders
    :: forall eff r a. (Respondable r, Requestable a)
    => Array RequestHeader -> String -> a -> Aff (ajax :: AJAX | eff) (AJ.AffjaxResponse r)
qlReqHeaders h p r =
    AJ.affjax $
        AJ.defaultRequest
            { url = p
            , method = POST
            , headers = h <> [ ContentType (MimeType "application/json") ]
            , content = Just r
            }

-- | A basic POST mechanism for QuickLift. Used for non-authenticated requests.
qlPost :: forall eff r a. (Respondable r, Requestable a)
      => String -> a -> Aff (ajax :: AJAX | eff) (AJ.AffjaxResponse r)
qlPost = qlReqHeaders []

-- | A POST mechanism for QuickLift that sends an authentication token to be
-- | checked by the server.
qlAuth
    :: forall eff r a. (Respondable r, Requestable a)
    => String -> String -> a -> Aff (ajax :: AJAX | eff) (AJ.AffjaxResponse r)
qlAuth token = qlReqHeaders [RequestHeader "auth" token]
