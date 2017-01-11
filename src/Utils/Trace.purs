module Utils.Trace where

import SlamData.Prelude

foreign import _addTime ∷ ∀ a b. (a → b → a × b) → (Unit → b) → (Number × b)

addTime ∷ ∀ a. (Unit → a) → Number × a
addTime = _addTime Tuple
