module BigPrelude
  ( module Prelude
  , module Data.Maybe
  , module Data.Foldable
  , module Data.Maybe.Unsafe
  , module Data.Either
  , module Control.Monad.Eff
  , module Optic.Lens
  , module Optic.Core
  , module Data.Functor
  , module Control.Alt
  , module Data.Tuple
  , module Control.Apply
  , module Control.Monad.Eff.Class
  , module Control.Plus
  , module Data.Enum
  , module Control.Bind
  , eitherToMaybe
  , eitherToList
  , eitherToArray
  , maybeToArray
  ) where

import Prelude
import Control.Bind
import Data.Enum
import Data.Foldable
import Control.Plus
import Control.Alt
import Control.Apply
import Data.Functor
import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Either
import qualified Data.Array as A
import qualified Data.List as L
import Data.List (List())
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Optic.Lens
import Optic.Core

eitherToMaybe :: forall a b. Either b a -> Maybe a
eitherToMaybe (Left _) =
  Nothing
eitherToMaybe (Right a) =
  Just a

eitherToList :: forall a b. Either b a -> List a
eitherToList (Left _) =
  L.Nil
eitherToList (Right a) =
  L.Cons a L.Nil

eitherToArray :: forall a b. Either b a -> Array a
eitherToArray (Left _) =
  []
eitherToArray (Right a) =
  [a]

maybeToArray :: forall a b. Maybe a -> Array a
maybeToArray = maybe [] pure
