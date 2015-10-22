module Util where

import BigPrelude

import Control.Monad.Aff (Aff())
import DOM (DOM())

foreign import replaceLocation :: forall e. String -> Aff (dom :: DOM | e) Unit
