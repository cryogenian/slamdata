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

module SlamData.Workspace.Card.Chart.BuildOptions.Pie where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array ((!!), (:))
import Data.Array as A
import Data.Int (toNumber)
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Math (floor)

import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (Key, PieBarData, keyCategory, buildChartAxes, buildPieBarData, saturateLast, printKey)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)


rowLength ∷ Int
rowLength = 4

buildPie ∷ M.Map JCursor Ax.Axis → ChartConfiguration → DSL OptionI
buildPie axes conf = do
  E.tooltip E.triggerItem

  E.series series

  E.legend do
    when (A.length legendNames > 20) E.hidden
    E.leftLeft
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.orient ET.Vertical
    E.items $ map ET.strItem legendNames

  E.colors colors
  where
  pieBarData ∷ PieBarData
  pieBarData = buildPieBarData $ buildChartAxes axes conf

  ks ∷ Array Key
  ks = A.fromFoldable $ M.keys pieBarData

  catVals ∷ Array String
  catVals = A.nub $ map keyCategory ks

  legendNames ∷ Array String
  legendNames =
    A.nub $ foldMap (pure ∘ printKey) $ M.keys pieBarData

  -- | group data by category if there is first series
  grouppedByCat ∷ Array (Array (Key × Number))
  grouppedByCat =
    A.groupBy (\(k × v) (k' × v') → (snd k $> fst k) ≡ (snd k' $> fst k'))
    $ A.fromFoldable
    $ M.toList pieBarData

  -- | group data by first series if there is second series
  grouppedBySeries ∷ Array (Array (Array (Key × Number)))
  grouppedBySeries =
    map (A.groupBy (\(k × v) (k' × v') →
                     ((snd k >>= snd) *> (snd k <#> fst)) ≡ ((snd k' >>= snd) *> (snd k' <#> fst))))
      grouppedByCat

  serie ∷ Array (Key × Number) → DSL ETP.PieSeriesI
  serie pairs = do
    traverse_ (E.name ∘ printKey ∘ saturateLast ∘ fst) $ A.head pairs
    E.buildItems $ for_ pairs \(key × value) →
      E.addItem do
        E.value value
        E.name $ printKey key
    E.label do
      E.normalLabel E.hidden
      E.emphasisLabel E.hidden
    E.labelLine do
      E.normalLabelLine E.hidden
      E.emphasisLabelLine E.hidden


  -- | Series groupped by category and series1
  serieGroups ∷ Array (Array (DSL ETP.PieSeriesI))
  serieGroups = map (map serie) grouppedBySeries

  -- | series matrix enumerated by first series and then by second series
  enumeratedSeries ∷ Array (Int × (Array (Int × (DSL ETP.PieSeriesI))))
  enumeratedSeries =
    A.zip (A.range 0 $ A.length serieGroups)
    $ map (\ss → A.zip (A.range 0 $ A.length ss) ss)
    $ serieGroups

  maxOneRow ∷ Int → Int → {r ∷ Number, center ∷ ET.Point }
  maxOneRow count ix =
    let r = 85.0 / toNumber count
        step = 100.0 / toNumber count
        modulus = toNumber $ mod ix count
        x = ET.Percent $ 55.0 + (modulus + 0.5 - (toNumber count) / 2.0) * step
        y = ET.Percent $ 55.0
        center = ET.Point { x, y }
    in {r, center}

  maxManyRows ∷ Int → Int → {r ∷ Number, center ∷ ET.Point }
  maxManyRows count ix =
    let
      nLength = toNumber rowLength
      r = 85.0 / nLength
      step = 85.0 / nLength
      modulus = toNumber $ mod ix count
      x = ET.Percent $ 55.0 + (modulus - nLength / 2.0 + 0.5) * step
      y = ET.Percent $ 1.2 * floor (toNumber ix / nLength) * r + r
      center = ET.Point {x, y}
    in {r, center}

  totalDonuts ∷ Int
  totalDonuts = A.length enumeratedSeries

  series ∷ ∀ i. DSL (pie ∷ ETP.I|i)
  series =
    for_ enumeratedSeries \(dIx × ss) →
      for_ ss \(pIx × pieCommands) → do
        let
          totalPies = A.length ss
          maxR =
            if totalPies <= rowLength
            then maxOneRow totalPies pIx
            else maxManyRows totalPies pIx
          step = maxR.r / toNumber (totalDonuts + 1)
        E.pie do
          pieCommands
          E.center maxR.center
          E.radius $ ET.Radius
            if totalDonuts ≡ 1
            then { start: ET.Pixel 0, end: ET.Percent maxR.r }
            else { start: ET.Percent (step * toNumber (dIx + 1))
                 , end: ET.Percent (step * toNumber (dIx + 2))
                 }
