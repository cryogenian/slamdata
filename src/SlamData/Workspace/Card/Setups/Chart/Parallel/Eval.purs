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

module SlamData.Workspace.Card.Setups.Chart.Parallel.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Parallel.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.List as L
import Data.StrMap as Sm
import Data.String as S

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Parallel))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Parallel.Model (ModelR, Model)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

import Utils.Array (enumerate)
import Utils.Foldable (enumeratedFor_)

type Item =
  { series ∷ String
  , dims ∷ Array Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  series ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "series"
  let
    ks ∷ Array String
    ks = map ("measure" ⊕ _) $ A.mapMaybe (S.stripPrefix $ S.Pattern "measure") $ Sm.keys obj

  dims ← for ks \k →
    map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? k

  pure { dims, series }

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildParallel

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  $ [ r.series # SCC.jcursorPrj # Sql.as "series" ]
  ⊕ ( map mkProjection $ enumerate r.dims )
  where
  mkProjection (ix × field) =
    field # SCC.jcursorPrj # Sql.as ("measure" ⊕ show ix) # SCC.applyTransform field

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy
  $ pure
  $ r.series # SCC.jcursorSql

buildParallel ∷ ModelR → Axes → Port.Port
buildParallel m axes =
  Port.ChartInstructions
    { options: pOptions axes m ∘ buildPData
    , chartType: Parallel
    }

buildPData ∷ JArray → Array Item
buildPData =
  foldMap $ foldMap A.singleton ∘ decodeItem

pOptions ∷ Axes → ModelR → Array Item → DSL OptionI
pOptions _ r pData = do
  E.parallel do
    E.left $ ET.Percent 5.0
    E.right $ ET.Percent 18.0
    E.bottom $ ET.Pixel 100

  E.colors colors
  E.series series

  E.parallelAxes axes

  when (A.length serieNames < 30) $ E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem serieNames

  where
  serieNames = map _.series pData

  series = for_ pData \serie → E.parallelSeries do
    E.name serie.series
    E.buildItems
      $ E.addItem
      $ E.buildValues
      $ for_ serie.dims E.addValue

  axes = enumeratedFor_ r.dims \(dimIx × dim) → E.addParallelAxis do
    E.dim dimIx
    E.name $ D.jcursorLabel dim
