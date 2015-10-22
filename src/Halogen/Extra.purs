module Halogen.Extra where

import Data.Monoid

import Halogen
import qualified Halogen.HTML.Indexed as H

-- | This slot convenience function takes the component, state, and slot
-- | constructor, and puts them in the slot. The argument order allows
-- | you to use the code in the following ways:
-- |
-- | ```purescript
-- | H.ul_ (map (E.slot todoItem todoInit <<< TodoSlot) state.todos)
-- | -- ...
-- | E.slot Counter.ui Counter.init (CounterSlot 0)
-- | ```
slot :: forall s f g p i
      . Component s f g
     -> s
     -> p
     -> HTML (SlotConstructor s f g p) i
slot comp state p = H.slot p \_ -> { component: comp, initialState: state }

-- | If your state is a monoid, and your initial state is simply `mempty`
-- | then you don't need to pass any state at all in with this combinator.
-- |
-- | ```purescript
-- | H.ul_ (map (E.mslot todoItem <<< TodoSlot) state.todos)
-- | ```
mslot :: forall s f g p i. (Monoid s)
      => Component s f g
      -> p
      -> HTML (SlotConstructor s f g p) i
mslot comp = slot comp mempty
