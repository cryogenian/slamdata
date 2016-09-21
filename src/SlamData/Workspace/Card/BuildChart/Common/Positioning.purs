module SlamData.Workspace.Card.BuildChart.Common.Positioning where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int

type RadialPosition r =
  { x ∷ Maybe Number
  , y ∷ Maybe Number
  , radius ∷ Maybe Number
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
