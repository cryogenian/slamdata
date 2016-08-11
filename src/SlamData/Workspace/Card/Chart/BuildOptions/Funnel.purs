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

module SlamData.Workspace.Card.Chart.BuildOptions.Funnel where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array ((!!))
import Data.Array as A
import Data.Function (on)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Int (toNumber)
import Math ((%))

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (ChartAxes, colors, buildChartAxes)

--sample data:
-- [ Tuple '' [Tuple 'dim1' 1.0, Tuple 'dim2' 5.0] ]
type FunnelData = Array (Tuple String (Array (Tuple String Number)))

buildFunnelData ∷ ChartAxes → FunnelData
buildFunnelData axes =
  A.fromFoldable
    $ map (map (A.fromFoldable
      -- output sample:
      -- ( Tuple ''
      --         ( Tuple 'dimA' 2
      --         : Tuple 'dimB' 10 )
      -- )
      <<< L.catMaybes
      <<< map combineDim
      -- output sample:
      -- ( Tuple ''
      --         ( (Tuple 'dimA' 1 : Tuple 'dimA' 1)
      --         : (Tuple 'dimB' 2 : Tuple 'dimB' 2 : Tuple 'dimB' 3 : Tuple 'dimB' 3) )
      -- )
      <<< L.groupBy ((==) `on` fst)
      <<< L.sortBy (compare `on` fst)
    ))
    -- output sample:
    -- ( Tuple ''
    --         ( Tuple 'dimA' 1 : Tuple 'dimB' 2
    --         : Tuple 'dimB' 3 : Tuple 'dimA' 1
    --         : Tuple 'dimB' 2 : Tuple 'dimB' 3 )
    -- )
    $ L.catMaybes
    $ map combineDup
    -- output sample:
    -- (( Tuple '' (Tuple 'dimA' 1) : Tuple '' (Tuple 'dimB' 2)
    --  : Tuple '' (Tuple 'dimB' 3) : Tuple '' (Tuple 'dimA' 1)
    --  : Tuple '' (Tuple 'dimB' 2) : Tuple '' (Tuple 'dimB' 3) ))
    $ L.groupBy ((==) `on` fst)
    $ L.sortBy (compare `on` fst)
    -- output sample:
    -- ( Tuple '' (Tuple 'dimA' 1) : Tuple '' (Tuple 'dimB' 2)
    -- : Tuple '' (Tuple 'dimB' 3) : Tuple '' (Tuple 'dimA' 1)
    -- : Tuple '' (Tuple 'dimB' 2) : Tuple '' (Tuple 'dimB' 3) )
    $ L.catMaybes
    $ map filterInvalid
    $ tagDuplications duplications
    $ L.zip dimensions values
  where
  dimensions ∷ List (Maybe String)
  dimensions = fromMaybe Nil $ axes.dimensions !! 0

  values ∷ List (Maybe Number)
  values = fromMaybe Nil $ axes.measures !! 0

  duplications ∷ List (Maybe String)
  duplications = fromMaybe Nil $ axes.series !! 0

  agg ∷ Maybe Aggregation
  agg = fromMaybe (Just Sum) $ join (axes.aggregations !! 0)

  tagDuplications
    ∷ List (Maybe String)
    → List (Tuple (Maybe String) (Maybe Number))
    → List (Tuple (Maybe String) (Tuple (Maybe String) (Maybe Number)))
  tagDuplications d v =
    if L.null d
      then map (Tuple Nothing) v
      else L.zip d v

  filterInvalid
    ∷ Tuple (Maybe String) (Tuple (Maybe String) (Maybe Number))
    → Maybe (Tuple String (Tuple String Number))
  filterInvalid (a × b × c) =
    case b, c of
      Just b', Just c' →
        Just $ Tuple (fromMaybe "" a) (Tuple b' c')
      _, _ → Nothing

  combineDup
    ∷ List (Tuple String (Tuple String Number))
    → Maybe (Tuple String (List (Tuple String Number)))
  combineDup x = do
    d ← fst <$> L.head x
    pure $ Tuple d $ map snd x

  combineDim
    ∷ List (Tuple String Number)
    → Maybe (Tuple String Number)
  combineDim x = do
    d ← fst <$> L.head x
    pure $ Tuple d (applyAggregation $ map snd x)

  applyAggregation ∷ List Number → Number
  applyAggregation = runAggregation (fromMaybe Sum agg)

buildFunnel
  ∷ Map JCursor Ax.Axis
  → String
  → String
  → ChartConfiguration
  → DSL OptionI
buildFunnel axes order align conf = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu sans"
      E.fontSize 12

  E.legend do
    E.items $ map ET.strItem dimNames
    E.topBottom
    E.textStyle do
      E.fontFamily "Ubuntu sans"

  E.colors colors

  E.titles
    $ traverse_ E.title titles

  E.series
    $ traverse_ (E.funnel ∘ serie numDup)
    $ A.zip (A.range 0 (A.length funnelData)) (funnelData)

  where
  dimNames ∷ Array String
  dimNames =
    A.nub
      $ A.concat
      $ map (map fst)
      $ map snd funnelData

  dupNames ∷ Array String
  dupNames = map fst funnelData

  numDup ∷ Int
  numDup = A.length dupNames

  titles ∷ Array (DSL ETP.TitleI)
  titles =
    map (mkTitle numDup) (A.range 0 $ numDup - 1)
    where
    mkTitle ∷ Int → Int → DSL ETP.TitleI
    mkTitle n i = do
      let
        -- max number of plot in one row: 4
        maxCol = 4
        nRow = ((n - 1) / maxCol + 1)
        nCol = if n <= maxCol then n else maxCol
        row = toNumber (i / nCol)
        col = (toNumber i) % (toNumber nCol)
        rowHight = 100.0 / (toNumber nRow)
        spaceCoeff = 0.9
      E.text $ fromMaybe "" $ dupNames !! i
      E.textStyle do
        E.fontFamily "Ubuntu sans"
        E.fontSize 12
      E.left
        $ ET.Percent
        $ 100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))
          -- adjust the position to the center
          - 0.3
      E.top
        $ ET.Percent
        $ 100.0 * (2.0 * row + 1.0) / (toNumber (nRow * 2))
          - (rowHight * spaceCoeff) / 2.0
      E.textCenter
      E.textBottom

  funnelData ∷ FunnelData
  funnelData = buildFunnelData $ buildChartAxes axes conf

  serie
    ∷ Int
    → Tuple Int (Tuple String (Array (Tuple String Number)))
    → DSL ETP.FunnelI
  serie n (i × (dup × a)) = do
    let
      -- max number of plot in one row: 4
      maxCol = 4
      nRow = ((n - 1) / maxCol + 1)
      nCol = if n <= maxCol then n else maxCol
      row = toNumber (i / nCol)
      col = (toNumber i) % (toNumber nCol)
      rowHight = 100.0 / (toNumber nRow)
      colWidth = 100.0 / (toNumber nCol)
      spaceCoeff = 0.75
    E.left
      $ ET.Percent
      $ 100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))
        - (colWidth * spaceCoeff) / 2.0
    E.top
      $ ET.Percent
      $ 100.0 * (2.0 * row + 1.0) / (toNumber (nRow * 2))
        - (rowHight * spaceCoeff) / 2.0
    E.widthPixelOrPercent
      $ ET.Percent
      $ colWidth * spaceCoeff
    E.heightPixelOrPercent
      $ ET.Percent
      $ rowHight * spaceCoeff
    when (dup ≠ "") $ E.name dup
    case order of
      "ascending" → E.ascending
      _ → E.descending
    case align of
      "left" → E.funnelLeft
      "right" → E.funnelRight
      _ → E.funnelCenter
    E.buildItems $ for_ a \x →
      E.addItem $ makeData x

  makeData
    ∷ Tuple String Number
    → DSL ETP.ItemI
  makeData (name × value) = do
    E.name name
    E.value value
