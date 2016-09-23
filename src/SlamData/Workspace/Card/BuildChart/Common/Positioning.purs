module SlamData.Workspace.Card.BuildChart.Common.Positioning where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP

type RadialPosition r =
  { x ∷ Maybe Number
  , y ∷ Maybe Number
  , radius ∷ Maybe Number
  | r
  }

type WithDonutRadius r =
  { radius ∷ Maybe {start ∷ Number, end ∷ Number }
  | r
  }

itemsInRow ∷ ∀ a. Array a → Int
itemsInRow arr =
  let
    len = A.length arr

    result
      | len < 2 = 1
      | len < 5 = 2
      | len < 10 = 3
      | otherwise = 4
  in
    result

rowCount ∷ ∀ a. Array a → Int
rowCount arr =
  Int.ceil $ Int.toNumber (A.length arr) / Int.toNumber (itemsInRow arr)

radialSpaceCoeff ∷ Number
radialSpaceCoeff = 0.85

adjustRadialPositions
  ∷ ∀ r
  . Array (RadialPosition r)
  → Array (RadialPosition r)
adjustRadialPositions ps =
  let
    inRow ∷ Int
    inRow = itemsInRow ps

    numRows ∷ Int
    numRows = rowCount ps

    topStep ∷ Number
    topStep =
      100.0 / Int.toNumber numRows

    radiusDivisor ∷ Number
    radiusDivisor = Int.toNumber $ max inRow numRows

    radius ∷ Maybe Number
    radius = Just $ 75.0 / radiusDivisor

    setPositions
      ∷ Array (RadialPosition r)
      → Int
      → Int
      → Int
      → Array (RadialPosition r)
      → Array (RadialPosition r)
    setPositions acc colIx rowIx inThisRow arr = case A.uncons arr of
      Nothing → acc
      Just {head, tail} →
        let
          top ∷ Maybe Number
          top = Just $ 100.0 * (2.0 * Int.toNumber rowIx + 1.0) / (Int.toNumber numRows * 2.0) - 10.0

          left ∷ Maybe Number
          left = Just $ 100.0 * (2.0 * Int.toNumber colIx + 1.0) / (Int.toNumber inThisRow * 2.0)

          toPush ∷ RadialPosition r
          toPush =
            head { x = left
                 , y = top
                 , radius = radius
                 }

          newAcc ∷ Array (RadialPosition r)
          newAcc = A.snoc acc toPush

          newColIx ∷ Int
          newColIx
            | one + colIx < inThisRow = colIx + one
            | otherwise = zero

          newRowIx ∷ Int
          newRowIx
            | newColIx ≡ zero = rowIx + one
            | otherwise = rowIx

          inNewRow ∷ Int
          inNewRow
            | A.length tail > inRow = inRow
            | newColIx ≠ zero = inRow
            | otherwise = A.length tail
        in
          setPositions newAcc newColIx newRowIx inNewRow tail
  in
    setPositions [] 0 0 inRow ps

adjustDonutRadiuses
  ∷ ∀ r
  . Array (WithDonutRadius r)
  → Array (WithDonutRadius r)
adjustDonutRadiuses arr =
  let
    len ∷ Number
    len = Int.toNumber $ A.length arr

    step ∷ Number
    step = 1.0 / len

    adjustRadius
      ∷ Array (WithDonutRadius r) → Number → Array (WithDonutRadius r) → Array (WithDonutRadius r)
    adjustRadius acc ix inp = case A.uncons inp of
      Nothing → acc
      Just {head, tail} →
        let
          toPush = head { radius = Just {start: ix * step, end: (ix + one) * step }}
          newAcc = A.snoc acc toPush
          newIx = ix + one
        in
          adjustRadius newAcc newIx tail
  in
    adjustRadius [] zero arr

radialTitles
  ∷ ∀ r i
  . Array (RadialPosition (name ∷ Maybe String |r))
  → DSL (title ∷ ETP.I|i)
radialTitles rposs = E.titles $ for_ rposs \{name, x, y, radius} → E.title do
  traverse_ E.text name
  E.textStyle do
    E.fontFamily "Ubuntu, sans"
    E.fontSize 12
  for_ x \left →
    E.left $ ET.Percent $ left - 0.3
  for_ y \top →
    E.top $ ET.Percent $ top - (rowHeight * radialSpaceCoeff) / 2.0
  E.textCenter
  E.textBottom
  where
  numRows ∷ Int
  numRows = rowCount rposs

  rowHeight ∷ Number
  rowHeight = 100.0 / Int.toNumber numRows


type RectangularPosition r =
  { x ∷ Maybe Number
  , y ∷ Maybe Number
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , fontSize ∷ Maybe Int
  | r
  }

adjustRectangularPositions
  ∷ ∀ r
  . Array (RectangularPosition r)
  → Array (RectangularPosition r)
adjustRectangularPositions arr =
  let
    len ∷ Int
    len = A.length arr

    inRow ∷ Int
    inRow = itemsInRow arr

    numRows ∷ Int
    numRows = rowCount arr

    topStep ∷ Number
    topStep =
      90.0 / Int.toNumber numRows

    setPositions
      ∷ Array (RectangularPosition r)
      → Int
      → Int
      → Int
      → Array (RectangularPosition r)
      → Array (RectangularPosition r)
    setPositions acc colIx rowIx inThisRow arr = case A.uncons arr of
      Nothing → acc
      Just {head, tail} →
        let
          top ∷ Number
          top = topStep * (Int.toNumber rowIx + 0.5) + 5.0

          leftStep ∷ Number
          leftStep = 90.0 / Int.toNumber inThisRow

          left ∷ Number
          left = leftStep * (Int.toNumber colIx + 0.5) + 5.0

          fontSize ∷ Int
          fontSize
            | numRows < 5 = 12
            | numRows < 6 = 11
            | otherwise = 10

          toPush ∷ RectangularPosition r
          toPush =
            head { x = Just left
                 , y = Just top
                 , w = Just $ 90.0 / Int.toNumber inRow
                 , h = Just $ 90.0 / Int.toNumber numRows
                 , fontSize = Just fontSize
                 }

          newAcc ∷ Array (RectangularPosition r)
          newAcc = A.snoc acc toPush

          newColIx ∷ Int
          newColIx
            | one + colIx < inThisRow = colIx + one
            | otherwise = zero

          newRowIx ∷ Int
          newRowIx
            | newColIx ≡ zero = rowIx + one
            | otherwise = rowIx

          inNewRow ∷ Int
          inNewRow
            | A.length tail > inRow = inRow
            | newColIx ≠ zero = inRow
            | otherwise = A.length tail
        in
          setPositions newAcc newColIx newRowIx inNewRow tail
  in
    setPositions [] 0 0 inRow arr


rectangularTitles
  ∷ ∀ r f i
  . Foldable f
  ⇒ f (RectangularPosition (name ∷ Maybe String |r))
  → DSL (title ∷ ETP.I|i)
rectangularTitles poss = E.titles $ for_ poss \{name, x, y, h, fontSize} → E.title do
  traverse_ E.text name
  E.textStyle do
    E.fontFamily "Ubuntu, sans"
    traverse_ E.fontSize fontSize
  traverse_ (E.top ∘ ET.Percent) y
  traverse_ (E.left ∘ ET.Percent) x
  E.textCenter
  E.textMiddle


rectangularGrids
  ∷ ∀ r f i
  . Foldable f
  ⇒ f (RectangularPosition r)
  → DSL (grid ∷ ETP.I|i)
rectangularGrids poss = E.grids $ for_ poss \{w, h, x, y} → E.grid do
  case x × w of
    Just x' × Just w' → E.left $ ET.Percent $ x' - w' / 2.0
    _ → pure unit
  case y × h of
    Just y' × Just h' → E.top $ ET.Percent $ y' - h' / 2.0
    _ → pure unit
  for_ w E.widthPct
  for_ h E.heightPct
