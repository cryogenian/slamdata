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

adjustRadialPositions
  ∷ ∀ r
  . Array (RadialPosition r)
  → Array (RadialPosition r)
adjustRadialPositions ps =
  let
    len ∷ Int
    len = A.length ps

    itemsInRow ∷ Int
    itemsInRow
      | len < 2 = 1
      | len < 5 = 2
      | len < 10 = 3
      | otherwise = 4

    numRows ∷ Int
    numRows =
      Int.ceil $ Int.toNumber len / Int.toNumber itemsInRow

    topStep ∷ Number
    topStep =
      90.0 / Int.toNumber numRows

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
          top ∷ Number
          top = topStep * (Int.toNumber rowIx + 0.5) + 5.0

          leftStep ∷ Number
          leftStep = 90.0 / Int.toNumber inThisRow

          left ∷ Number
          left = leftStep * (Int.toNumber colIx + 0.5) + 5.0

          toPush ∷ RadialPosition r
          toPush =
            head { x = Just left
                 , y = Just top
                 , radius = Just $ 90.0 / Int.toNumber itemsInRow
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
            | A.length tail > itemsInRow = itemsInRow
            | newColIx ≠ zero = itemsInRow
            | otherwise = A.length tail
        in
          setPositions newAcc newColIx newRowIx inNewRow tail
  in
    setPositions [] 0 0 itemsInRow ps

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
  ∷ ∀ r f i
  . Foldable f
  ⇒ f (RadialPosition (name ∷ Maybe String |r))
  → DSL (title ∷ ETP.I|i)
radialTitles rposs = E.titles $ for_ rposs \{name, x, y, radius} → E.title do
  traverse_ E.text name
  E.textStyle do
    E.fontFamily "Ubuntu, sans"
    E.fontSize 12
  traverse_ (E.top ∘ ET.Percent) y
  traverse_ (E.left ∘ ET.Percent) x
  E.textCenter
  E.textBottom


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

    itemsInRow ∷ Int
    itemsInRow
      | len < 2 = 1
      | len < 5 = 2
      | len < 10 = 3
      | otherwise = 4

    numRows ∷ Int
    numRows =
      Int.ceil $ Int.toNumber len / Int.toNumber itemsInRow

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
                 , w = Just $ 90.0 / Int.toNumber itemsInRow
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
            | A.length tail > itemsInRow = itemsInRow
            | newColIx ≠ zero = itemsInRow
            | otherwise = A.length tail
        in
          setPositions newAcc newColIx newRowIx inNewRow tail
  in
    setPositions [] 0 0 itemsInRow arr


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
  for_ x $ E.left ∘ ET.Percent
  for_ y $ E.top ∘ ET.Percent
  for_ w E.widthPct
  for_ h E.heightPct
