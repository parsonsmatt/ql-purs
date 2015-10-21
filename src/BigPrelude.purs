module BigPrelude
  ( module Prelude
  , module Data.Maybe
  , module Data.Either
  , module Control.Monad.Eff
  , module Data.List
  , eitherToMaybe
  , eitherToList
  ) where

import Prelude
import Data.Maybe
import Data.List
import Data.Either
import Control.Monad.Eff

eitherToMaybe :: forall a b. Either b a -> Maybe a
eitherToMaybe (Left _) =
  Nothing
eitherToMaybe (Right a) =
  Just a

eitherToList :: forall a b. Either b a -> List a
eitherToList (Left _) =
  Nil
eitherToList (Right a) =
  Cons a Nil
