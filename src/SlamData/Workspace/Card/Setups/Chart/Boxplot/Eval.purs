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

module SlamData.Workspace.Card.Setups.Chart.Boxplot.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Boxplot.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array ((!!))
import Data.Array as A
import Data.List as L
import Data.List ((:))
import Data.Lens ((.~))
import Data.Map as M
import Data.Int as Int
import Data.Set as Set
import Data.NonEmpty as NE

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Boxplot))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning (rectangularGrids, rectangularTitles, adjustRectangularPositions)
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquare as Sql

import Utils.Foldable (enumeratedFor_)

type Item =
  { dimension ∷ String
  , value ∷ Number
  , series ∷ Maybe String
  , parallel ∷ Maybe String
  }

type OnOneBoxplot =
  { name ∷ Maybe String
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , fontSize ∷ Maybe Int
  , series ∷ Array BoxplotSeries
  }

type BoxplotSeries =
  { name ∷ Maybe String
  , items ∷ String >> (Array Number × BoxplotItem)
  }

type BoxplotItem = Maybe
  { low ∷ Number
  , q1 ∷ Number
  , q2 ∷ Number
  , q3 ∷ Number
  , high ∷ Number
  }

boundIQR ∷ Maybe Number
boundIQR = Just 1.5

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  dimension ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "dimension"
  value ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "value"
  series ← map Sem.maybeString $ obj .? "series"
  parallel ← map Sem.maybeString $ obj .? "parallel"
  pure { dimension
       , value
       , series
       , parallel
       }

eval ∷ ∀ m. BCE.ChartSetupEval ModelR m
eval m =
  BCE.chartSetupEval
    (\md fp →
      addOrderBy $ SCC.buildBasicSql buildProjections (const Nothing) md fp
    ) buildBoxplot m
  where
  fields ∷ ModelR → L.List Sql.Sql
  fields r =
    L.fromFoldable $ A.catMaybes
      [ r.parallel <#> SCC.jcursorSql
      , r.series <#> SCC.jcursorSql
      , Just $ r.dimension # SCC.jcursorSql
      ]
  orderBy ∷ Maybe (Sql.OrderBy Sql.Sql)
  orderBy = m >>= \r → case fields r of
    L.Nil → Nothing
    x : xs → Just $ Sql.OrderBy $ Tuple Sql.ASC <$> NE.NonEmpty x xs

  addOrderBy ∷ Sql.Sql → Sql.Sql
  addOrderBy =
    (Sql._Select ∘ Sql._orderBy .~ orderBy)
    ∘ (Sql._Select ∘ Sql._isDistinct .~ true)

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.value # SCC.jcursorPrj # Sql.as "value"
  , r.dimension # SCC.jcursorPrj # Sql.as "dimension"
  , r.series # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "series"
  , r.parallel # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "parallel"
  ]

buildBoxplot ∷ ModelR → Axes → JArray → Port.Port
buildBoxplot m axes jarr =
  Port.ChartInstructions
    { options: boxplotOptions m axes $ buildBoxplotData jarr
    , chartType: Boxplot
    }

buildBoxplotData ∷ JArray → Array OnOneBoxplot
buildBoxplotData =
  adjustRectangularPositions
  ∘ oneBoxplots
  ∘ foldMap (foldMap A.singleton ∘ decodeItem)
  where
  oneBoxplots ∷ Array Item → Array OnOneBoxplot
  oneBoxplots =
    BCE.groupOn _.parallel
      ⋙ map \(name × is) →
        { name
        , x: Nothing
        , y: Nothing
        , w: Nothing
        , h: Nothing
        , fontSize: Nothing
        , series: boxplotSeries is
        }

  boxplotSeries ∷ Array Item → Array BoxplotSeries
  boxplotSeries =
    BCE.groupOn _.series
      ⋙ map \(name × is) →
        { name
        , items: boxplotPoints is
        }

  boxplotPoints ∷ Array Item → String >> (Array Number × BoxplotItem)
  boxplotPoints =
    M.fromFoldable
    ∘ map (map analyzeBoxplot)
    ∘ BCE.groupOn _.dimension

  analyzeBoxplot ∷ Array Item → Array Number × BoxplotItem
  analyzeBoxplot is
    | A.null is = [] × Nothing
    | otherwise =
      let
        arr ∷ Array Number
        arr = map _.value is

        sortedArr ∷ Array Number
        sortedArr = A.sort arr

        quantile ∷ Array Number → Number → Maybe Number
        quantile inp p = do
          let
            h = Int.toNumber (A.length inp - one) * p + one
            h' = Int.floor h
            v = arr !! (h' - one)
            v' = inp !! h'
            e = h - Int.toNumber h'
          if e > 0.0
            then case v × v' of
              Just jv × Just jv' → Just $ jv + e * (jv' - jv)
              _ → Nothing
            else v

        q1 = fromMaybe zero $ quantile sortedArr 0.25
        q2 = fromMaybe zero $ quantile sortedArr 0.5
        q3 = fromMaybe zero $ quantile sortedArr 0.75
        iqr = q3 - q1
        low = case boundIQR of
          Just b → q1 - b * iqr
          _ → fromMaybe zero $ A.head sortedArr
        high = case boundIQR of
          Just b → q3 + b * iqr
          _ → fromMaybe zero $ A.last sortedArr
        outliers ∷ Array Number
        outliers = A.filter (\x → x < low ∨ x > high) sortedArr
      in
       outliers × Just {low, q1, q2, q3, high}
