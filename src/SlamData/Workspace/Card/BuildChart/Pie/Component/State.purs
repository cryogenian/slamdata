module SlamData.Workspace.Card.BuildChart.Pie.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Pie.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Pie.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , category ∷ Select JCursor
  , value ∷ Select JCursor
  , valueAggregation ∷ Select Aggregation
  , valueAggregationOpen ∷ Boolean
  , donut ∷ Select JCursor
  , parallel ∷ Select JCursor
  , pickerOptions ∷ Maybe PickerOptions
  }

type PickerOptions =
  { options ∷ Array JCursor
  , select ∷ Selection (Const Unit)
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , category: emptySelect
  , value: emptySelect
  , valueAggregation: emptySelect
  , valueAggregationOpen: false
  , donut: emptySelect
  , parallel: emptySelect
  , pickerOptions: Nothing
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_value ∷ ∀ r a. LensP { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAggregation ∷ ∀ r a. LensP { valueAggregation ∷ a | r } a
_valueAggregation = lens _.valueAggregation _{ valueAggregation = _ }

_valueAggregationOpen ∷ ∀ r a. LensP { valueAggregationOpen ∷ a | r } a
_valueAggregationOpen = lens _.valueAggregationOpen _{ valueAggregationOpen = _ }

_category ∷ ∀ r a. LensP { category ∷ a | r } a
_category = lens _.category _{ category = _ }

_donut ∷ ∀ r a. LensP { donut ∷ a | r } a
_donut = lens _.donut _{ donut = _ }

_parallel ∷ ∀ r a. LensP { parallel ∷ a | r } a
_parallel = lens _.parallel _{ parallel = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { pickerOptions = Just { options, select: f (Const unit) } }
