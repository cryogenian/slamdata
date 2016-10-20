module Utils.Foldable where

import SlamData.Prelude

enumeratedFor_
  ∷ ∀ a b f m
  . Applicative m, Foldable f
  ⇒ f a
  → (Int × a → m b)
  → m Unit
enumeratedFor_ fl fn =
  foldr (\a (ix × action) → (ix + one) × ((fn a) *> action)) (zero × pure unit) fl
