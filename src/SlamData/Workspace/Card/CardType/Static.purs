module SlamData.Workspace.Card.CardType.Static where

import SlamData.Prelude

import Data.Variant (inj, on)

import Halogen.HTML as H

import Unsafe.Coerce (unsafeCoerce)

_static = SProxy ∷ SProxy "static"

type StaticR r = (static ∷ Unit|r)
type Static r = Variant (StaticR r)

all ∷ ∀ r. Array (Static r)
all = [ static ]

static ∷ ∀ r. Variant (static ∷ Unit|r)
static = inj _static unit

eq_ ∷ ∀ r rr. (Variant r → Variant rr → Boolean) → Static r → Static rr → Boolean
eq_ cb r = cb (unsafeCoerce r) # on _static (on _static tt ff r)

print ∷ ∀ r. (Variant r → String) → Static r → String
print cb = cb # on _static (const "static")

encode ∷ ∀ r. (Variant r → String) → Static r → String
encode cb = cb # on _static (const "static-setup")

icon ∷ ∀ r. (Variant r → String) → Static r → String
icon cb = cb # on _static (const "setupFormInput/static")

name ∷ ∀ r. (Variant r → String) → Static r → String
name cb = cb # on _static (const "Static Text")

parse ∷ ∀ r. String → String ⊹ Static r
parse = case _ of
  "static" → Right static
  _ → Left "this is not static text card type"

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Static r → Boolean
consumerInteractable cb = cb # on _static ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Static r → Array H.ClassName
cardClasses cb = cb # on _static (const [ H.ClassName "sd-form-input-setup" ] )
