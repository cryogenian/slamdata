module Utils.Foldable where

import SlamData.Prelude

enumeratedFor_
  ∷ ∀ a b f m ix
  . (Applicative m, Foldable f, Semiring ix)
  ⇒ f a
  → (ix × a → m b)
  → m Unit
enumeratedFor_ fl fn =
  snd
  $ foldr (\a (ix × action) →
            (ix + one)
            × ((fn $ ix × a) *> action))
    (zero × pure unit) fl
