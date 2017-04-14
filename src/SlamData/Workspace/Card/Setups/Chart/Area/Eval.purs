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

module SlamData.Workspace.Card.Setups.Chart.Area.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Area.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, put, get)
import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array ((!!))
import Data.Array as A
import Data.Function (on)
import Data.Lens ((^?), _Just, (^.), (.~), (?~))
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.NonEmpty as NE
import Data.Path.Pathy as Path
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Chart.Area.Model (Model, ModelR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Area))
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors, getShadeColor)
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

import SqlSquare as Sql

import Utils.Array (enumerate)
import Utils.Path as PU

type Item =
  { dimension ∷ String
  , measure ∷ Number
  , series ∷ Maybe String
  }

type AreaSeries =
  Int × { name ∷ Maybe String
        , items ∷ String >> Number
        }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  measure ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "measure"
  dimension ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "dimension"
  series ← map Sem.maybeString $ obj .? "series"
  pure { measure
       , dimension
       , series
       }

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , MonadAsk CEM.CardEnv m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.Resource
  → m Port.Port
eval m resource = do
  records × axes ← BCE.analyze resource =<< get
  put $ Just $ CEM.Analysis { resource, records, axes }
  case m of
    Nothing → CEM.throw "Incorrect setup area model"
    Just r → do
      let
        path = resource ^. Port._filePath
        backendPath = fromMaybe Path.rootDir $ Path.parentDir path
      results ←
        CEM.liftQ $ QQ.query backendPath $ buildSql r path
      pure $ buildArea axes results r


buildSql ∷ ModelR → PU.FilePath → Sql.Sql
buildSql r path =
  Sql.buildSelect
    $ ( Sql._projections .~ buildProjections r )
    ∘ ( Sql._relations ?~ (Sql.TableRelation { path: Left path, alias: Nothing } ))
    ∘ ( Sql._groupBy ?~ buildGroupBy r )

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r =
  ( applyMeasure $ Sql.projection measureF # Sql.as "measure" )
  : ( Sql.projection dimensionF # Sql.as "dimension" )
  : ( Sql.projection seriesF # Sql.as "series" )
  : L.Nil
  where
  measureF ∷ Sql.Sql
  measureF =
    fromMaybe Sql.null $ map QQ.jcursorToSql $ r.value ^? D._value ∘ D._projection

  dimensionF ∷ Sql.Sql
  dimensionF =
    fromMaybe Sql.null $ map QQ.jcursorToSql $ r.dimension ^? D._value ∘ D._projection

  seriesF ∷ Sql.Sql
  seriesF =
    fromMaybe Sql.null $ map QQ.jcursorToSql $ r.series ^? _Just ∘ D._value ∘ D._projection

  applyMeasure ∷ Sql.Projection Sql.Sql → Sql.Projection Sql.Sql
  applyMeasure p = case r.value ^? D._value ∘ D._transform ∘ _Just of
    Nothing → p
    Just t → T.applyTransform t p


buildGroupBy ∷ ModelR → Sql.GroupBy Sql.Sql
buildGroupBy r = Sql.GroupBy
  { keys, having: Nothing }
  where
  keys =
    L.fromFoldable $ join
    [ foldMap (A.singleton ∘ QQ.jcursorToSql) $ r.dimension ^? D._value ∘ D._projection
    , foldMap (A.singleton ∘ QQ.jcursorToSql) $ r.series ^? _Just ∘ D._value ∘ D._projection
    ]

buildArea ∷ Ax.Axes → JArray → ModelR → Port.Port
buildArea axes jarr m =
  Port.ChartInstructions
    { options: areaOptions axes m $ buildAreaData jarr
    , chartType: Area
    }

buildAreaData ∷ Array Json → Array AreaSeries
buildAreaData jarr =
  let
    items ∷ Array Item
    items = foldMap (foldMap A.singleton ∘ decodeItem) jarr

    seriesFn ∷ Array Item → Array { name ∷ Maybe String, items ∷ Array Item }
    seriesFn is =
      let
        groupped =
          map (NE.fromNonEmpty A.cons)
          $ A.groupBy (eq `on` _.series) is

        recordify a =
          { name: A.head a >>= _.series, items: a }
      in
       map recordify groupped

    mappify ∷ Array Item → String >> Number
    mappify = foldMap \{ dimension, measure } → M.singleton dimension measure
  in
    enumerate $ seriesFn items <#> \{ name, items } →
      { name
      , items: mappify items
      }

areaOptions ∷ Ax.Axes → ModelR → Array AreaSeries → DSL OptionI
areaOptions axes r areaData = do
  let
    cols =
      [ { label: D.jcursorLabel r.dimension, value: CCT.formatValueIx 0 }
      , { label: D.jcursorLabel r.value, value: CCT.formatValueIx 1 }
      ]
    opts = flip foldMap r.series \dim →
      [ { label: D.jcursorLabel dim, value: _.seriesName } ]

  E.tooltip do
    E.formatterAxis (CCT.tableFormatter (pure ∘ _.color) (cols <> opts))
    E.triggerAxis
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.lineAxisPointer
      E.lineStyle do
        E.width 1
        E.solidLine

  E.yAxis do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1

  E.xAxis do
    E.axisType xAxisConfig.axisType
    traverse_ E.interval $ xAxisConfig.interval
    case xAxisConfig.axisType of
      ET.Category →
        E.items $ map ET.strItem xValues
      _ → pure unit
    E.disabledBoundaryGap
    E.axisTick do
      E.length 2
      E.lineStyle do
        E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.axisLine $ E.lineStyle do
      E.width 1
    E.axisLabel do
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.colors colors

  E.grid BCP.cartesian

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  xAxisType ∷ Ax.AxisType
  xAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType <$> (r.dimension ^? D._value ∘ D._projection) <*> pure axes

  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration xAxisType

  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType xAxisType

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (snd ⋙_.items ⋙ M.keys ⋙ Set.fromFoldable)
        areaData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (snd ⋙ _.name ⋙ Set.fromFoldable)
        areaData

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = for_ areaData \(ix × serie) → E.line do
    E.buildItems $ for_ xValues \key → do
      case M.lookup key serie.items of
        Nothing → E.missingItem
        Just v → E.addItem do
          E.name key
          E.buildValues do
            E.addStringValue key
            E.addValue v
    for_ serie.name E.name
    for_ (colors !! ix) \color → do
      E.itemStyle $ E.normal $ E.color color
      E.areaStyle $ E.normal $ E.color $ getShadeColor color (if r.isStacked then 1.0 else 0.5)
    E.lineStyle $ E.normal $ E.width 2
    E.symbol ET.Circle
    E.symbolSize 0
    E.smooth r.isSmooth
    when r.isStacked $ E.stack "stack"
