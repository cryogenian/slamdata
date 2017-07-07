module SlamData.Workspace.Card.CardType.Input where

import SlamData.Prelude

import Data.Variant (inj, on)

import Halogen.HTML as H

import Unsafe.Coerce (unsafeCoerce)

_text = SProxy ∷ SProxy "text"
_numeric = SProxy ∷ SProxy "numeric"
_date = SProxy ∷ SProxy "date"
_time = SProxy ∷ SProxy "time"
_datetime = SProxy ∷ SProxy "datetime"

type InputR r =
  ( text ∷ Unit
  , numeric ∷ Unit
  , date ∷ Unit
  , time ∷ Unit
  , datetime ∷ Unit
  | r)

type Input r = Variant (InputR r)

all ∷ ∀ r. Array (Input r)
all =
  [ text
  , numeric
  , date
  , time
  , datetime
  ]

text ∷ ∀ r. Variant (text ∷ Unit|r)
text = inj _text unit

numeric ∷ ∀ r. Variant (numeric ∷ Unit|r)
numeric = inj _numeric unit

date ∷ ∀ r. Variant (date ∷ Unit|r)
date = inj _date unit

time ∷ ∀ r. Variant (time ∷ Unit|r)
time = inj _time unit

datetime ∷ ∀ r. Variant (datetime ∷ Unit|r)
datetime = inj _datetime unit

eq_ ∷ ∀ r rr. (Variant r → Variant rr → Boolean) → Input r → Input rr → Boolean
eq_ cb r = cb (unsafeCoerce r)
  # on _text (on _text tt ff r)
  # on _numeric (on _numeric tt ff r)
  # on _date (on _date tt ff r)
  # on _time (on _time tt ff r)
  # on _datetime (on _datetime tt ff r)

print ∷ ∀ r. (Variant r → String) → Input r → String
print cb = cb
  # on _text (const "text")
  # on _numeric (const "numeric")
  # on _date (const "date")
  # on _time (const "time")
  # on _datetime (const "datetime")

name ∷ ∀ r. (Variant r → String) → Input r → String
name cb = cb
  # on _text (const "Text Input")
  # on _numeric (const "Numeric Input")
  # on _date (const "Date Input")
  # on _time (const "Time Input")
  # on _datetime (const "Date/Time Input")

parse ∷ ∀ r. String → String ⊹ Input r
parse = case _ of
  "text" → Right text
  "numeric" → Right numeric
  "date" → Right date
  "time" → Right time
  "datetime" → Right datetime
  _ → Left "this is not input card type"

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Input r → Boolean
consumerInteractable cb = cb
  # on _text ff
  # on _numeric ff
  # on _date ff
  # on _time ff
  # on _datetime ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Input r → Array H.ClassName
cardClasses cb = cb
  # on _text clss
  # on _numeric clss
  # on _date clss
  # on _time clss
  # on _datetime clss
  where
  clss _ = [ H.ClassName "sd-form-input-setup" ]
