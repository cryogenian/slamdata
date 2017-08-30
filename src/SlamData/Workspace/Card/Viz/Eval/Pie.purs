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

module SlamData.Workspace.Card.Viz.Eval.Pie where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Lens ((?~))
import Data.Map as Map
import Data.Set as Set
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Common.Positioning (adjustRadialPositions, adjustDonutRadiuses, RadialPosition, WithDonutRadius, radialTitles)
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Viz.Model as M
import SqlSquared as Sql
import Utils.SqlSquared (all, asRel, variRelation)

type OnePieSeries =
  RadialPosition
  ( series ∷ Array DonutSeries
  , name ∷ Maybe String
  )

type DonutSeries =
  WithDonutRadius
  ( name ∷ Maybe String
  , items ∷ String >> Number
  )

type Item =
  { category ∷ String
  , measure ∷ Number
  , donut ∷ Maybe String
  , parallel ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  category ← Sem.requiredString "" <$> obj .? "category"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  donut ← Sem.maybeString <$> obj .? "donut"
  parallel ← Sem.maybeString <$> obj .? "parallel"
  pure { category, measure, donut, parallel }

eval
  ∷ ∀ m
  . VizEval m
  ( M.ChartModel
  → Port.ChartInstructionsPort
  → m ( Port.Resource × DSL OptionI )
  )
eval m { chartType, dimMap, aux, axes } = do
  var × resource ← CEM.extractResourcePair Port.Initial

  let
    sql = buildSql (M.getEvents m) var

  CEM.CardEnv { path, varMap } ← ask

  outResource ←
    CE.liftQ $ CEC.localEvalResource (Sql.Query empty sql) varMap
  records ←
    CE.liftQ $ CEC.sampleResource path outResource Nothing

  let
    items = buildData records
    options = buildOptions dimMap items
  pure $ outResource × options

buildSql ∷ Array M.FilteredEvent → Port.Var → Sql.Sql
buildSql es var =
  Sql.buildSelect
  $ all
  ∘ (Sql._relations
     ?~ (variRelation (unwrap var) # asRel "res"))

buildData ∷ Array Json → Array OnePieSeries
buildData =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> onePies
    >>> adjustPositions

  where
  adjustPositions ∷ Array OnePieSeries → Array OnePieSeries
  adjustPositions =
    adjustRadialPositions
      >>> map \pie → pie { series = adjustDonutRadiuses pie.series }

  onePies ∷ Array Item → Array OnePieSeries
  onePies =
    BCE.groupOn _.parallel
      >>> map \(name × is) →
            { name
            , x: Nothing
            , y: Nothing
            , radius: Nothing
            , series: donuts is
            }

  donuts ∷ Array Item → Array DonutSeries
  donuts =
    BCE.groupOn _.donut
      >>> map \(name × is) →
            { name
            , radius: Nothing
            , items: Map.fromFoldable (toPoint <$> is)
            }

  toPoint ∷ Item → Tuple String Number
  toPoint { category, measure } = category × measure

buildOptions ∷ P.DimMap → Array OnePieSeries → DSL OptionI
buildOptions dimMap pieData = do
  let
    mkRow prj value  = P.lookup prj dimMap # foldMap \dim →
      [ { label: D.jcursorLabel dim, value } ]

    cols = A.fold
      [ mkRow P.category $ CCT.formatAssocProp "key"
      , mkRow P.value $ CCT.formatAssocProp "value"
      , mkRow P.donut _.seriesName
      ]

  CCT.tooltip do
    E.formatterItem (CCT.tableFormatter (Just ∘ _.color) cols ∘ pure)
    E.triggerItem

  E.colors colors

  E.legend do
    E.textStyle do
      E.fontSize 12
      E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem legendNames
    E.orient ET.Vertical
    E.leftLeft

  E.series series

  radialTitles pieData
    $ maybe "" D.jcursorLabel
    $ P.lookup P.parallel dimMap

  where
  itemNames ∷ Array String
  itemNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.items
                            ⋙ Map.keys
                            ⋙ Set.fromFoldable)
                )
        pieData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (_.series ⋙ foldMap (_.name ⋙ Set.fromFoldable)) pieData

  legendNames ∷ Array String
  legendNames
    | A.null seriesNames = itemNames
    | otherwise = do
      s ← seriesNames
      i ← itemNames
      pure $ s ⊕ ":" ⊕ i

  series ∷ ∀ i. DSL (pie ∷ ETP.I|i)
  series = for_ pieData \{x, y, radius: parallelR, series: ss} →
    for_ ss \{radius, items, name} → E.pie do
      E.selectedMode ET.Multiple
      E.label do
        E.normal E.hidden
        E.emphasis E.hidden

      E.buildCenter do
        traverse_ (E.setX ∘ E.percents) x
        traverse_ (E.setY ∘ E.percents) y

      for_ parallelR \pR →
        for_ radius \{start, end} → E.buildRadius do
          E.setStart $ E.percents $ start * pR
          E.setEnd $ E.percents $ end * pR

      for_ name E.name

      E.buildItems $ for_ (asList $ Map.toUnfoldable $ items) \(key × value) →
        E.addItem do
          E.value value
          E.name $ foldMap (flip append ":") name ⊕ key
          BCE.assoc { key, value }
