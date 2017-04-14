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

module SlamData.Workspace.Card.Setups.Chart.Bar.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Bar.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, put, get)
import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Function (on)
import Data.Array as A
import Data.Map as M
import Data.NonEmpty as NE
import Data.Set as Set
import Data.Lens ((^?), _Just, (^.), (.~), (?~))
import Data.List ((:))
import Data.List as L
import Data.Path.Pathy as Path

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Bar))
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Chart.Bar.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Transform as T

import SqlSquare as Sql

import Utils.Path as PU

type BarSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

type BarStacks =
  { stack ∷ Maybe String
  , series ∷ Array BarSeries
  }

type Item =
  { measure ∷ Number
  , category ∷ String
  , parallel ∷ Maybe String
  , stack ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  measure ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "measure"
  category ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "category"
  parallel ← map Sem.maybeString $ obj .? "parallel"
  stack ← map Sem.maybeString $ obj .? "stack"
  pure { measure
       , category
       , parallel
       , stack
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
    Nothing → CEM.throw "Incorrect setup bar model"
    Just r → do
      let
        path = resource ^. Port._filePath
        backendPath = fromMaybe Path.rootDir $ Path.parentDir path
      results ←
        CEM.liftQ $ QQ.query backendPath $ buildSql r path
      pure $ buildBar axes results r

buildSql ∷ ModelR → PU.FilePath → Sql.Sql
buildSql r path =
  Sql.buildSelect
    $ ( Sql._projections .~ buildProjections r )
    ∘ ( Sql._relations ?~ (Sql.TableRelation { path: Left path, alias: Nothing } ))
    ∘ ( Sql._groupBy ?~ buildGroupBy r )

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r =
  ( applyMeasure $ Sql.projection measureF # Sql.as "measure" )
  : ( Sql.projection categoryF # Sql.as "category" )
  : ( Sql.projection stackF # Sql.as "stack" )
  : ( Sql.projection parallelF # Sql.as "parallel" )
  : L.Nil
  where
  measureF ∷ Sql.Sql
  measureF =
    fromMaybe Sql.null $ map QQ.jcursorToSql $ r.value ^? D._value ∘ D._projection

  categoryF ∷ Sql.Sql
  categoryF =
    fromMaybe Sql.null $ map QQ.jcursorToSql $ r.category ^? D._value ∘ D._projection

  stackF ∷ Sql.Sql
  stackF =
    fromMaybe Sql.null $ map QQ.jcursorToSql $ r.stack ^? _Just ∘ D._value ∘ D._projection

  parallelF ∷ Sql.Sql
  parallelF =
    fromMaybe Sql.null $ map QQ.jcursorToSql $ r.parallel ^? _Just ∘ D._value ∘ D._projection

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
    [ foldMap (A.singleton ∘ QQ.jcursorToSql) $ r.category ^? D._value ∘ D._projection
    , foldMap (A.singleton ∘ QQ.jcursorToSql) $ r.stack ^? _Just ∘ D._value ∘ D._projection
    , foldMap (A.singleton ∘ QQ.jcursorToSql) $ r.parallel ^? _Just ∘ D._value ∘ D._projection
    ]

buildBar ∷ Axes → JArray → ModelR → Port.Port
buildBar axes jarr m =
  Port.ChartInstructions
    { options: barOptions axes m $ buildBarData jarr
    , chartType: Bar
    }

buildBarData ∷ Array Json → Array BarStacks
buildBarData jarr =
  let
    items ∷ Array Item
    items = foldMap (foldMap A.singleton ∘ decodeItem) jarr

    stacksFn ∷ Array Item → Array { stack ∷ Maybe String, series ∷ Array Item }
    stacksFn is  =
      let
        groupped =
          map (NE.fromNonEmpty A.cons)
          $ A.groupBy (eq `on` _.stack) is
        recordify a =
          { stack: A.head a >>= _.stack, series: a }
      in
        map recordify groupped

    seriesFn ∷ Array Item → Array { name ∷ Maybe String, items ∷ Array Item }
    seriesFn is =
      let
        groupped =
          map (NE.fromNonEmpty A.cons)
          $ A.groupBy (eq `on` _.parallel) is

        recordify a =
          { name: A.head a >>= _.parallel, items: a }
      in
        map recordify groupped

    mappify ∷ Array Item → String >> Number
    mappify = foldMap \{ measure, category } → M.singleton category measure
  in
    stacksFn items <#> \{stack, series} →
      { stack
      , series: seriesFn series <#> \{name, items} →
          { name
          , items: mappify items
          }
      }

barOptions ∷ Axes → ModelR → Array BarStacks → DSL OptionI
barOptions axes r barData = do
  let
    cols =
      [ { label: D.jcursorLabel r.category, value: CCT.formatValueIx 0 }
      , { label: D.jcursorLabel r.value, value: CCT.formatValueIx 1 }
      ]
    seriesFn dim = [ { label: D.jcursorLabel dim, value: _.seriesName } ]
    opts = foldMap seriesFn if isJust r.parallel then r.parallel else r.stack

  E.tooltip do
    E.formatterAxis (CCT.tableFormatter (pure ∘ _.color) (cols <> opts))
    E.textStyle $ E.fontSize 12
    E.triggerAxis

  E.colors colors

  E.xAxis do
    E.axisType ET.Category
    E.enabledBoundaryGap
    E.items $ map ET.strItem xValues
    E.axisLabel do
      traverse_ E.interval xAxisConfig.interval
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.yAxis do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle $ E.width 1
    E.splitLine $ E.lineStyle $ E.width 1

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    case xAxisConfig.axisType of
      ET.Category | A.length seriesNames > 40 → E.hidden
      _ → pure unit
    E.items $ map ET.strItem seriesNames
    E.leftLeft
    E.topBottom

  E.grid BCP.cartesian

  E.series series

  where
  xAxisType ∷ Ax.AxisType
  xAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType <$> (r.category ^? D._value ∘ D._projection) <*> pure axes


  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration xAxisType

  seriesNames ∷ Array String
  seriesNames = case r.parallel of
    Just _ →
      A.fromFoldable
      $ flip foldMap barData
      $ foldMap (Set.fromFoldable ∘ _.name)
      ∘ _.series
    Nothing →
      A.catMaybes $ map _.stack barData

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ flip foldMap barData
      $ foldMap (Set.fromFoldable ∘ M.keys ∘ _.items)
      ∘ _.series


  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType xAxisType

  series ∷ ∀ i. DSL (bar ∷ ETP.I|i)
  series = for_ barData \stacked →
    for_ stacked.series \serie → E.bar do
      E.buildItems $ for_ xValues \key →
        case M.lookup key serie.items of
          Nothing → E.missingItem
          Just v → E.addItem do
            E.name key
            E.buildValues do
              E.addStringValue key
              E.addValue v
      case r.parallel of
        Just _ → do
          for_ stacked.stack E.stack
          for_ serie.name E.name
        Nothing → do
          E.stack "default stack"
          for_ stacked.stack E.name
