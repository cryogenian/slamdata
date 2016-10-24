module SlamData.Workspace.Card.BuildChart.Candlestick.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Candlestick.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Candlestick.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  , dimension ∷ Select JCursor
  , high ∷ Select JCursor
  , highAgg ∷ Select Aggregation
  , low ∷ Select JCursor
  , lowAgg ∷ Select Aggregation
  , open ∷ Select JCursor
  , openAgg ∷ Select Aggregation
  , close ∷ Select JCursor
  , closeAgg ∷ Select Aggregation
  , parallel ∷ Select JCursor
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , picker: Nothing
  , dimension: emptySelect
  , high: emptySelect
  , highAgg: emptySelect
  , low: emptySelect
  , lowAgg: emptySelect
  , open: emptySelect
  , openAgg: emptySelect
  , close: emptySelect
  , closeAgg: emptySelect
  , parallel: emptySelect
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_dimension ∷ ∀ r a. LensP { dimension ∷ a | r} a
_dimension = lens _.dimension _{ dimension = _ }

_high ∷ ∀ r a. LensP { high ∷ a | r} a
_high = lens _.high _{ high = _ }

_highAgg ∷ ∀ r a. LensP { highAgg ∷ a | r} a
_highAgg = lens _.highAgg _{ highAgg = _ }

_low ∷ ∀ r a. LensP { low ∷ a | r} a
_low = lens _.low _{ low = _ }

_lowAgg ∷ ∀ r a. LensP { lowAgg ∷ a | r} a
_lowAgg = lens _.lowAgg _{ lowAgg = _ }

_open ∷ ∀ r a. LensP { open ∷ a | r} a
_open = lens _.open _{ open = _ }

_openAgg ∷ ∀ r a. LensP { openAgg ∷ a | r} a
_openAgg = lens _.openAgg _{ openAgg = _ }

_close ∷ ∀ r a. LensP { close ∷ a | r} a
_close = lens _.close _{ close = _ }

_closeAgg ∷ ∀ r a. LensP { closeAgg ∷ a | r} a
_closeAgg = lens _.closeAgg _{ closeAgg = _ }

_parallel ∷ ∀ r a. LensP { parallel ∷ a | r} a
_parallel = lens _.parallel _{ parallel = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
