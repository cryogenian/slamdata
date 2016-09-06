module SlamData.Workspace.Card.Chart.BuildOptions.Gauge where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Map as M
import Data.Int as Int

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)

infixr 3 type M.Map as >>

type GaugeR =
  { value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , parallel ∷ Maybe JCursor
  , multiple ∷ Maybe JCursor
  , axes ∷ Ax.Axes
  }

type GaugeItem =
  { name ∷ Maybe String
  , value ∷ Number
  }

type GaugeSerie =
  { x ∷ Maybe Number
  , y ∷ Maybe Number
  , radius ∷ Maybe Number
  , name ∷ Maybe String
  , items ∷ Array GaugeItem
  }


buildGaugeData ∷ GaugeR → JArray → Array GaugeSerie
buildGaugeData r records = series
  where
  valueAndSizeMap ∷ Maybe String >> Maybe String >> Array Number
  valueAndSizeMap =
    foldl foldFn M.empty records

  foldFn
    ∷ Maybe String >> Maybe String >> Array Number
    → Json
    → Maybe String >> Maybe String >> Array Number
  foldFn acc js =
    let
       mbParallel = toString =<< flip cursorGet js =<< r.parallel
       mbMultiple = toString =<< flip cursorGet js =<< r.multiple
       values = foldMap A.singleton $ toNumber =<< cursorGet r.value js

       alterFn
         ∷ Maybe (Maybe String >> Array Number)
         → Maybe (Maybe String >> Array Number)
       alterFn Nothing = Just $ M.singleton mbMultiple values
       alterFn (Just multiple) =
         Just $ M.alter alterMultiple mbMultiple multiple

       alterMultiple
         ∷ Maybe (Array Number)
         → Maybe (Array Number)
       alterMultiple Nothing = Just values
       alterMultiple (Just arr) = Just $ arr ⊕ values
    in
      M.alter alterFn mbParallel acc

  mkSerie ∷ Maybe String × (Maybe String >> Array Number) → Array GaugeSerie
  mkSerie (name × mp) =
    let
      pairs ∷ Array (Maybe String × Array Number)
      pairs = A.fromFoldable $ M.toList mp
      items = pairs <#> \(name × values) →
        { name
        , value: Ag.runAggregation r.valueAggregation values
        }

    in [ { radius: Nothing
         , items
         , x: Nothing
         , y: Nothing
         , name
         }
       ]

  unpositionedSeries ∷ Array GaugeSerie
  unpositionedSeries =
    foldMap mkSerie $ M.toList valueAndSizeMap

  series ∷ Array GaugeSerie
  series =
    let
      len = A.length unpositionedSeries

      itemsInRow
        | len < 2 = 1
        | len < 5 = 2
        | len < 10 = 3
        | otherwise = 4

      numRows =
        Int.ceil $ Int.toNumber len / Int.toNumber itemsInRow

      topStep =
        90.0 / Int.toNumber numRows

      setPositions
        ∷ Array GaugeSerie
        → Int
        → Int
        → Int
        → Array GaugeSerie
        → Array GaugeSerie
      setPositions acc colIx rowIx inThisRow arr = traceAny inThisRow \_ → case A.uncons arr of
        Nothing → acc
        Just {head, tail} →
          let
            top = topStep * ((Int.toNumber rowIx) + 0.5) + 5.0

            leftStep = 90.0 / Int.toNumber inThisRow

            left = leftStep * ((Int.toNumber colIx) + 0.5) + 5.0

            toPush =
              head{ x = Just left
                  , y = Just top
                  , radius = Just $ 90.0 / Int.toNumber itemsInRow
                  }

            newAcc = A.snoc acc toPush

            newColIx
              | one + colIx < inThisRow = colIx + one
              | otherwise = zero

            newRowIx
              | newColIx ≡ zero = rowIx + one
              | otherwise = rowIx

            inNewRow
              | A.length tail > itemsInRow = itemsInRow
              | newColIx ≠ zero = itemsInRow
              | otherwise = A.length tail
          in
            setPositions newAcc newColIx newRowIx inNewRow tail
    in
      setPositions [] 0 0 itemsInRow unpositionedSeries

buildGauge ∷ GaugeR → JArray → DSL OptionI
buildGauge r records = do
  E.tooltip do
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.colors colors

  E.series $ for_ series \serie → E.gauge do
    for_ serie.name E.name
    E.axisLine $ E.lineStyle $ E.setWidth $ E.pixels 10
    E.splitLine $ E.length 20
    traverse_ (E.buildGaugeRadius ∘ E.percents) serie.radius

    E.buildCenter do
      traverse_ (E.setX ∘ E.percents) serie.x
      traverse_ (E.setY ∘ E.percents) serie.y

    when (A.length serie.items > 1)
      $ E.title E.hidden

    E.detail do
      traverse_ E.formatterString serie.name
      when (A.length series < 2 ∧ A.length serie.items > 1) E.hidden
      E.buildOffsetCenter do
        E.setX $ E.percents zero
        E.setY $ E.percents 65.0
      E.textStyle do
        E.fontSize 16
        E.fontFamily "Ubuntu, sans"
        for_ (A.head colors) E.color


    if (A.length allValues > 1)
      then do
      for_ (F.minimum allValues) E.min
      for_ (F.maximum allValues) E.max
      else
      for_ (A.head allValues) \v → do
        E.min $ v / 2.0
        E.max $ v * 1.5

    E.buildItems
      $ for_ serie.items \item → E.addItem do
        E.value item.value
        traverse_ E.name item.name

  where
  series ∷ Array GaugeSerie
  series = buildGaugeData r records

  allValues ∷ Array Number
  allValues = map _.value $ A.concatMap _.items series
