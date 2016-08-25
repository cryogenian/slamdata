{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Form.Select.Component where

import SlamData.Prelude

import Data.Array (length, range, zipWith, singleton)
import Data.Int as Int
import Data.Lens ((^.))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Form.Select as S

data Query s a
  = Choose Int a
  | SetSelect (S.Select s) a
  | GetValue (Maybe s → a)
  | GetSelect (S.Select s → a)
  | ToggleOpened a

type SelectConfig r =
  { disableWhen ∷ Int → Boolean
  , defaultWhen ∷ Int → Boolean
  , ariaLabel ∷ Maybe String
  , defaultOption ∷ String
  | r
  }

primarySelect
  ∷ ∀ a
  . (S.OptionVal a)
  ⇒ Maybe String
  → H.Component (S.Select a) (Query a) Slam
primarySelect mbLabel =
  select { disableWhen: (_ < 2)
         , defaultWhen: (_ > 1)
         , ariaLabel: mbLabel
         , defaultOption: "Select axis source" }

secondarySelect
  ∷ forall a
  . (S.OptionVal a)
  ⇒ Maybe String
  → H.Component (S.Select a) (Query a) Slam
secondarySelect mbLabel =
  select { disableWhen: (_ < 1)
         , defaultWhen: const true
         , ariaLabel: mbLabel
         , defaultOption: "Select axis source" }

select
  ∷ ∀ a r
  . S.OptionVal a
  ⇒ SelectConfig r
  → H.Component (S.Select a) (Query a) Slam
select config =
  H.component { render: render config, eval }

render
  ∷ ∀ a r
  . S.OptionVal a
  ⇒ SelectConfig r
  → S.Select a
  → H.ComponentHTML (Query a)
render config state =
  HH.select
    ([ HP.classes [ B.formControl ]
       -- `fromJust` is safe here because we know that value are `show`n ints
     , HE.onValueChange (HE.input (Choose ∘ unsafePartial fromJust <<< Int.fromString))
     , HP.disabled $ config.disableWhen len
     ]
    <> maybe [] (singleton <<< ARIA.label) config.ariaLabel)
    (defOption <> (zipWith (option selected) opts (range 0 len)))
  where
  len ∷ Int
  len = length opts

  opts ∷ Array a
  opts = state ^. S._options

  selected ∷ Maybe a
  selected = state ^. S._value

  defOption ∷ Array (H.ComponentHTML (Query a))
  defOption =
    if config.defaultWhen len
    then singleton $ defaultOption selected
    else [ ]

  defaultOption ∷ Maybe a → H.ComponentHTML (Query a)
  defaultOption val =
    HH.option
      [ HP.selected (val == Nothing)
      , HP.value "-1"
      ]
      [ HH.text config.defaultOption ]

  option ∷ Maybe a → a → Int → H.ComponentHTML (Query a)
  option currentVal val i =
    HH.option
      [ HP.selected (pure val == currentVal)
      , HP.value (show i)
      ]
      [ HH.text (S.stringVal val) ]

eval ∷ ∀ a. Eq a ⇒ Query a ~> H.ComponentDSL (S.Select a) (Query a) Slam
eval (Choose i next) = H.modify (S.trySelect i) $> next
eval (SetSelect s next) = H.set s $> next
eval (GetValue continue) = map continue $ H.gets (_ ^. S._value)
eval (GetSelect continue) = map continue H.get
eval (ToggleOpened next) = pure next
