{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Chart.Common.Tooltip where

import SlamData.Prelude
import Color as Color
import CSS as CSS
import Data.Array as A
import Data.Foreign (F, Foreign, readNumber, typeOf) as Frn
import Data.Foreign.Index (index) as Frn
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer as VDS
import Unsafe.Coerce (unsafeCoerce)

import Utils (showFormattedNumber, hush')

type ColumnFormatter a =
  { label ∷ String
  , value ∷ a → String
  }

type Color = String

tableFormatterPar ∷ ∀ a. (a → Maybe String) → Array (Array (ColumnFormatter a)) → Array a → String
tableFormatterPar getColor ts as =
  foldMap (uncurry (tableFormatter getColor))
    $ A.zipWith (\a b → a × pure b) ts as

tableFormatter ∷ ∀ a. (a → Maybe String) → Array (ColumnFormatter a) → Array a → String
tableFormatter getColor cols as = VDS.render absurd (unwrap table)
  where
  table =
    HH.table
      [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
      ([ headers ] <> map row as)

  headers = HH.tr_ ([ header "" ] <> map (header ∘ _.label) cols)
  header h = HH.th_ [ HH.text h ]
  row r = HH.tr_ ([ color (getColor r) ] <> map (cell r) cols)
  cell r f = HH.td_ [ HH.text (f.value r) ]

  color = case _ of
    Nothing → HH.text ""
    Just c →
      HH.td_
        [ HH.span
            [ HP.class_ $ HH.ClassName "sd-chart-tooltip-color"
            , HC.style $ for_ (Color.fromHexString c) CSS.backgroundColor
            ]
            []
        ]

type FormatterInput r = { value ∷ Frn.Foreign | r }

valueIx ∷ ∀ a r. (Frn.Foreign → Frn.F a) → Int → FormatterInput r → Maybe a
valueIx read ix inp = hush' (read =<< Frn.index ix inp.value)

formatValueIx ∷ ∀ r. Int → FormatterInput r → String
formatValueIx ix = maybe "" formatForeign ∘ valueIx pure ix

formatNumberValueIx ∷ ∀ r. Int → FormatterInput r → String
formatNumberValueIx ix = showFormattedNumber ∘ fromMaybe zero ∘ valueIx Frn.readNumber ix

formatNumber ∷ ∀ r. FormatterInput r → String
formatNumber = showFormattedNumber ∘ fromMaybe zero ∘ hush' ∘ Frn.readNumber ∘ _.value

formatForeign ∷ Frn.Foreign → String
formatForeign val = case Frn.typeOf val of
  "string"  → (unsafeCoerce ∷ Frn.Foreign → String) val
  "boolean" → show $ (unsafeCoerce ∷ Frn.Foreign → Boolean) val
  "number"  → showFormattedNumber $ (unsafeCoerce ∷ Frn.Foreign → Number) val
  _         → ""
