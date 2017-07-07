module SlamData.Workspace.Card.CardType.Select where

import SlamData.Prelude

import Data.Variant (inj, on, case_)

import Halogen.HTML as H

import Unsafe.Coerce (unsafeCoerce)

_dropdown = SProxy ∷ SProxy "dropdown"
_checkbox = SProxy ∷ SProxy "checkbox"
_radio = SProxy ∷ SProxy "radio"

type SelectR r =
  ( dropdown ∷ Unit
  , checkbox ∷ Unit
  , radio ∷ Unit
  | r)

type Select r = Variant (SelectR r)

all ∷ ∀ r. Array (Select r)
all = [ dropdown, checkbox, radio ]

dropdown ∷ ∀ r. Variant (dropdown ∷ Unit|r)
dropdown = inj _dropdown unit

checkbox ∷ ∀ r. Variant (checkbox ∷ Unit|r)
checkbox = inj _checkbox unit

radio ∷ ∀ r. Variant (radio ∷ Unit|r)
radio = inj _radio unit

eq_ ∷ ∀ r rr. (Variant r → Variant rr → Boolean) → Select r → Select rr → Boolean
eq_ cb r = cb (unsafeCoerce r)
  # on _dropdown (on _dropdown tt ff r)
  # on _checkbox (on _checkbox tt ff r)
  # on _radio (on _radio tt ff r)

print ∷ ∀ r. (Variant r → String) → Select r → String
print cb = cb
  # on _dropdown (const "dropdown")
  # on _checkbox (const "checkbox")
  # on _radio (const "radio")

name ∷ ∀ r. (Variant r → String) → Select r → String
name cb = cb
  # on _dropdown (const "Dropdown")
  # on _checkbox (const "Checkbox Group")
  # on _radio (const "Radio Group")

parse ∷ ∀ r. String → String ⊹ Select r
parse = case _ of
  "dropdown" → Right dropdown
  "checkbox" → Right checkbox
  "radio" → Right radio
  _ → Left "this is not select card type"

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Select r → Boolean
consumerInteractable cb = cb
  # on _dropdown ff
  # on _radio ff
  # on _checkbox ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Select r → Array H.ClassName
cardClasses cb = cb
  # on _dropdown clss
  # on _radio clss
  # on _checkbox clss
  where
  clss _ = [ H.ClassName "sd-form-input-setup" ]

maximumCountOfEntries ∷ Select () → Int
maximumCountOfEntries = case_
  # on _dropdown (const 1000)
  # on _radio (const 1000)
  # on _checkbox (const 1000)

maximumCountOfSelectedValues ∷ Select () → Int
maximumCountOfSelectedValues = case_
  # on _dropdown (const 1)
  # on _radio (const 1)
  # on _checkbox (const top)
