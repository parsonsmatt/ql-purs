module Util where

import BigPrelude

import DOM (DOM())

foreign import replaceLocation :: forall e. String -> Eff (dom :: DOM | e) Unit
