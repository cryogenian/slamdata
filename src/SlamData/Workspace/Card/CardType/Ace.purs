module SlamData.Workspace.Card.CardType.Ace where

import SlamData.Prelude

import Data.Variant (inj, on)

import Halogen.HTML as H
import Halogen.HTML as HH

import Unsafe.Coerce (unsafeCoerce)

_aceMarkdown = SProxy ∷ SProxy "aceMarkdown"
_aceSql = SProxy ∷ SProxy "aceSql"

aceMarkdown ∷ ∀ r. Variant (aceMarkdown ∷ Unit|r)
aceMarkdown = inj _aceMarkdown unit

aceSql ∷ ∀ r. Variant (aceSql ∷ Unit|r)
aceSql = inj _aceSql unit

type AceR r =
  ( aceMarkdown ∷ Unit
  , aceSql ∷ Unit
  | r)

type Ace r = Variant (AceR r)

name ∷ ∀ r. (Variant r → String) → Ace r → String
name cb = cb
  # on _aceMarkdown (const "Setup Markdown")
  # on _aceSql (const "Query")

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Ace r → Array H.ClassName
cardClasses cb = cb
  # on _aceMarkdown (const $ [ HH.ClassName "sd-card-ace", HH.ClassName "sd-card-markdown" ])
  # on _aceSql (const $ [ HH.ClassName "sd-card-ace", HH.ClassName "sd-card-sql" ])

mode ∷ ∀ r. (Variant r → String) → Ace r → String
mode cb = cb
  # on _aceMarkdown (const "ace/mode/markdown")
  # on _aceSql (const "ace/mode/sql")

eq_ ∷ ∀ r rr. (Variant r → Variant rr → Boolean) → Ace r → Ace rr → Boolean
eq_ cb r = cb (unsafeCoerce r)
  # on _aceMarkdown (on _aceMarkdown tt ff r)
  # on _aceSql (on _aceSql tt ff r)

print ∷ ∀ r. (Variant r → String) → Ace r → String
print cb = cb
  # on _aceMarkdown (const "ace-markdown")
  # on _aceSql (const "ace-sql")

parse ∷ ∀ r. String → String ⊹ Ace r
parse = case _ of
  "ace-markdown" → Right aceMarkdown
  "ace-sql" → Right aceSql
  _ → Left "this is not ace card"

icon ∷ ∀ r. (Variant r → String) → Ace r → String
icon cb = cb
  # on _aceMarkdown (const "setupMarkdown")
  # on _aceSql (const "query")

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Ace r → Boolean
consumerInteractable cb = cb
  # on _aceMarkdown ff
  # on _aceSql ff
