module SlamData.Notebook.Deck.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Notebook.Cell.Component (CellQueryP, CellStateP)
import SlamData.Notebook.Cell.CellId (CellId)
import SlamData.Notebook.Deck.BackSide.Component as Back


newtype CellSlot = CellSlot CellId

derive instance genericCellSlot :: Generic CellSlot
instance eqCellSlot :: Eq CellSlot where eq = gEq
instance ordCellSlot :: Ord CellSlot where compare = gCompare

type BackSideSlot = Unit

type ChildSlot =
  Either CellSlot BackSideSlot

type ChildQuery =
  Coproduct CellQueryP Back.Query

type ChildState =
  Either CellStateP Back.State


cpCell
  ∷ ChildPath
      CellStateP ChildState
      CellQueryP ChildQuery
      CellSlot ChildSlot
cpCell = cpL

cpBackSide
  ∷ ChildPath
      Back.State ChildState
      Back.Query ChildQuery
      BackSideSlot ChildSlot
cpBackSide = cpR
