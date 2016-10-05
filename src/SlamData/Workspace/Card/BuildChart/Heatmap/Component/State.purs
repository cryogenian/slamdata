module SlamData.Workspace.Card.BuildChart.Heatmap.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Form.Select (Select, emptySelect)
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.ColorScheme (ColorScheme)
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Inputs (Select', PickerOptions)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type State =
   { axes ∷ Axes
   , levelOfDetails ∷ LevelOfDetails
   , minValue ∷ Number
   , maxValue ∷ Number
   , isSchemeReversed ∷ Boolean
   , abscissa ∷ Select JCursor
   , ordinate ∷ Select JCursor
   , value ∷ Select JCursor
   , valueAgg ∷ Select' Aggregation
   , series ∷ Select JCursor
   , colorScheme ∷ Select' ColorScheme
   , picker ∷ Maybe (PickerOptions JCursor Selection)
   }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , minValue: zero
  , maxValue: one
  , isSchemeReversed: false
  , abscissa: emptySelect
  , ordinate: emptySelect
  , value: emptySelect
  , valueAgg: false × emptySelect
  , series: emptySelect
  , colorScheme: false × emptySelect
  , picker: Nothing
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_abscissa ∷ ∀ a r. LensP { abscissa ∷ a | r } a
_abscissa = lens _.abscissa _{ abscissa = _ }

_ordinate ∷ ∀ a r. LensP { ordinate ∷ a | r } a
_ordinate = lens _.ordinate _{ ordinate = _ }

_value ∷ ∀ a r. LensP { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ a r. LensP { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_series ∷ ∀ a r. LensP { series ∷ a | r} a
_series = lens _.series _{ series = _ }

_colorScheme ∷ ∀ a r. LensP { colorScheme ∷ a | r } a
_colorScheme = lens _.colorScheme _{ colorScheme = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just {options, select: f (Const unit) } }
