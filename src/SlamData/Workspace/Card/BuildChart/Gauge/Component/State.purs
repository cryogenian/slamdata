module SlamData.Workspace.Card.BuildChart.Gauge.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Gauge.Component.ChildSlot as GCS
import SlamData.Workspace.Card.BuildChart.Gauge.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (Select', PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , value ∷ Select JCursor
  , valueAgg ∷ Select' Aggregation
  , multiple ∷ Select JCursor
  , parallel ∷ Select JCursor
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , value: emptySelect
  , valueAgg: false × emptySelect
  , multiple: emptySelect
  , parallel: emptySelect
  , picker: Nothing
  }

type StateP =
  ParentState State GCS.ChildState QueryC GCS.ChildQuery Slam GCS.ChildSlot

_value ∷ ∀ r a. LensP { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. LensP { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_multiple ∷ ∀ r a. LensP { multiple ∷ a | r } a
_multiple = lens _.multiple _{ multiple = _ }

_parallel ∷ ∀ r a. LensP { parallel ∷ a | r } a
_parallel = lens _.parallel _{ parallel = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
