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

module SlamData.Workspace.Card.Setups.DimensionMap.Component.State where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Lens (Prism', Lens', _Just, lens, (^.), (.~), (?~), (^?))
import Data.List as L
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as Tr
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Package.DSL as T
import SlamData.Workspace.Card.Setups.Package.Projection as Pr

type Package = T.Package M.AnyCardModel (Set.Set J.JCursor)

interpret ∷ ∀ m a. Prism' M.AnyCardModel m → T.PackageM m a → Package
interpret prsm dsl =
  T.onPrism prsm $ T.interpret axesComposer dsl

type State =
  { axes ∷ Ax.Axes
  , dimMap ∷ T.DimensionMap
  , selected ∷ Maybe (T.Projection ⊹ T.Projection)
  }

initialState ∷ ∀ a. a → State
initialState _ =
  { axes: Ax.initialAxes
  , dimMap: T.emptyDimMap
  , selected: Nothing
  }

_axes ∷ ∀ a r. Lens' { axes ∷ a | r } a
_axes = lens _.axes _{ axes = _ }

_selected ∷ ∀ a r. Lens' { selected ∷ a | r } a
_selected = lens _.selected _{ selected = _ }

_dimMap ∷ ∀ a r. Lens' { dimMap ∷ a | r } a
_dimMap = lens _.dimMap _{ dimMap = _ }

axesComposer ∷ T.AxesComposer (Set.Set J.JCursor)
axesComposer =
  { filter, guard }
  where
  filter ∷ T.Projection → T.DimensionMap → Set.Set J.JCursor → Set.Set J.JCursor
  filter prj dimMap s =
    maybe s (flip Set.delete s)
      $ dimMap ^? T.unpackProjection prj ∘ _Just ∘ D._value ∘ D._projection

  guard ∷ T.Projection → T.DimensionMap → Set.Set J.JCursor → Set.Set J.JCursor
  guard prj dimMap s =
    case dimMap ^? T.unpackProjection prj ∘ _Just ∘ D._value ∘ D._projection of
      Nothing → Set.empty
      _ → s

projectionCursors ∷ T.Projection → Package → State → Set.Set J.JCursor
projectionCursors prj pack state =
  fromMaybe Set.empty
    $ pack.cursorMap state.dimMap state.axes
    ^. T.unpackProjection prj

selectedCursors ∷ Package → State → L.List J.JCursor
selectedCursors pack state = case state.selected of
  Just (Left lns) → L.fromFoldable $ projectionCursors lns pack state
  _ → L.Nil

isDisabled ∷ T.Projection → Package → State → Boolean
isDisabled prj pack state =
  Set.isEmpty $ projectionCursors prj pack state

isConfigurable ∷ T.Projection → Package → State → Boolean
isConfigurable prj pack state =
  let
    axis = state.dimMap ^? T.unpackProjection prj ∘ _Just ∘ D._value ∘ D._projection
  in maybe false (eq Ax.Measure) $ Ax.axisType <$> axis <*> pure state.axes


transforms ∷ State → Array Tr.Transform
transforms _ = Tr.aggregationTransforms

setValue ∷ T.Projection → J.JCursor → State → State
setValue fld v =
  _dimMap ∘ T.unpackProjection fld ?~ wrapFn v
  where
  wrapFn = fromMaybe D.projection $ wrapFns ^. T.unpackProjection fld
  wrapFns =
    SM.empty
      # (T.unpackProjection Pr._dimension ?~ D.projection)
      ∘ (T.unpackProjection Pr._high ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (T.unpackProjection Pr._low ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (T.unpackProjection Pr._open ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (T.unpackProjection Pr._close ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (T.unpackProjection Pr._parallel ?~ D.projection)
      ∘ (T.unpackProjection Pr._value ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (T.unpackProjection Pr._series ?~ D.projection)
      ∘ (T.unpackProjection Pr._category ?~ D.projection)
      ∘ (T.unpackProjection Pr._stack ?~ D.projection)
      ∘ (T.unpackProjection Pr._source ?~ D.projection)
      ∘ (T.unpackProjection Pr._target ?~ D.projection)
      ∘ (T.unpackProjection Pr._abscissa ?~ D.projection)
      ∘ (T.unpackProjection Pr._ordinate ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (T.unpackProjection Pr._secondValue ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (T.unpackProjection Pr._donut ?~ D.projection)
      ∘ (T.unpackProjection Pr._multiple ?~ D.projection)

showValue ∷ T.Projection → Maybe J.JCursor → String
showValue fld c = do
  fromMaybe "" $ map showJCursor c <|> (values ^. T.unpackProjection fld)
  where
  values =
    SM.empty
      # (T.unpackProjection Pr._dimension ?~ "Select dimension")
      ∘ (T.unpackProjection Pr._high ?~ "Select high")
      ∘ (T.unpackProjection Pr._low ?~ "Select low")
      ∘ (T.unpackProjection Pr._open ?~ "Select open")
      ∘ (T.unpackProjection Pr._close ?~ "Select close")
      ∘ (T.unpackProjection Pr._parallel ?~ "Select parallel")
      ∘ (T.unpackProjection Pr._value ?~ "Select measure")
      ∘ (T.unpackProjection Pr._series ?~ "Select series")
      ∘ (T.unpackProjection Pr._category ?~ "Select category")
      ∘ (T.unpackProjection Pr._stack ?~ "Select stack")
      ∘ (T.unpackProjection Pr._source ?~ "Select source")
      ∘ (T.unpackProjection Pr._target ?~ "Select target")
      ∘ (T.unpackProjection Pr._abscissa ?~ "Select X-Axis")
      ∘ (T.unpackProjection Pr._ordinate ?~ "Select Y-Axis")
      ∘ (T.unpackProjection Pr._secondValue ?~ "Select the second measure")
      ∘ (T.unpackProjection Pr._donut ?~ "Select donut")
      ∘ (T.unpackProjection Pr._multiple ?~ "Select multiple")

chooseLabel ∷ T.Projection → String
chooseLabel fld = fromMaybe "" $ labels ^. T.unpackProjection fld
  where
  labels =
    SM.empty
      # (T.unpackProjection Pr._dimension ?~ "Choose dimension")
      ∘ (T.unpackProjection Pr._high ?~ "Choose measure for high position")
      ∘ (T.unpackProjection Pr._low ?~ "Choose measure for close position")
      ∘ (T.unpackProjection Pr._open ?~ "Choose measure for open position")
      ∘ (T.unpackProjection Pr._close ?~ "Choose measure for close position")
      ∘ (T.unpackProjection Pr._parallel ?~ "Choose measure for low position")
      ∘ (T.unpackProjection Pr._value ?~ "Choose measure")
      ∘ (T.unpackProjection Pr._series ?~ "Choose series")
      ∘ (T.unpackProjection Pr._category ?~ "Choose category")
      ∘ (T.unpackProjection Pr._stack ?~ "Choose stack")
      ∘ (T.unpackProjection Pr._source ?~ "Choose source")
      ∘ (T.unpackProjection Pr._target ?~ "Choose target")
      ∘ (T.unpackProjection Pr._abscissa ?~ "Choose X-Axis")
      ∘ (T.unpackProjection Pr._ordinate ?~ "Choose Y-Axis")
      ∘ (T.unpackProjection Pr._secondValue ?~ "Choose the second measure")
      ∘ (T.unpackProjection Pr._donut ?~ "Choose donut")


showDefaultLabel ∷ T.Projection → Maybe J.JCursor → String
showDefaultLabel fld c =
  fromMaybe "" $ (labels ^. T.unpackProjection fld) <|> map showJCursor c
  where
  labels =
    SM.empty
      # (T.unpackProjection Pr._dimension ?~ "Dimension label")
      ∘ (T.unpackProjection Pr._high ?~ "High position label")
      ∘ (T.unpackProjection Pr._low ?~ "Low position label")
      ∘ (T.unpackProjection Pr._open ?~ "Open position label")
      ∘ (T.unpackProjection Pr._close ?~ "Close position label")
      ∘ (T.unpackProjection Pr._parallel ?~ "Parallel label")
      ∘ (T.unpackProjection Pr._value ?~ "Measure label")
      ∘ (T.unpackProjection Pr._series ?~ "Series label")
      ∘ (T.unpackProjection Pr._category ?~ "Category label")
      ∘ (T.unpackProjection Pr._stack ?~ "Stack label")
      ∘ (T.unpackProjection Pr._source ?~ "Source label")
      ∘ (T.unpackProjection Pr._target ?~ "Target label")
      ∘ (T.unpackProjection Pr._abscissa ?~ "X-Axis label")
      ∘ (T.unpackProjection Pr._ordinate ?~ "Y-Axis label")
      ∘ (T.unpackProjection Pr._secondValue ?~ "Measure#2 label")
      ∘ (T.unpackProjection Pr._donut ?~ "Donut label")
      ∘ (T.unpackProjection Pr._multiple ?~ "Multiple label")

setTransform ∷ T.Projection → Maybe Tr.Transform → State → State
setTransform fld t = _dimMap ∘ T.unpackProjection fld ∘ _Just ∘ D._value ∘ D._transform .~ t

setLabel ∷ T.Projection → String → State → State
setLabel fld str = _dimMap ∘ T.unpackProjection fld ∘ _Just ∘ D._category ?~ D.Static str

clear ∷ T.Projection → State → State
clear fld = _dimMap ∘ T.unpackProjection fld .~ Nothing

select ∷ T.Projection → State → State
select fld = _selected .~ (Just $ Left fld)

configure ∷ T.Projection → State → State
configure fld = _selected .~ (Just $ Right fld)

deselect ∷ State → State
deselect = _selected .~ Nothing

getSelected ∷ T.Projection → State → Maybe D.LabeledJCursor
getSelected fld state = state ^. _dimMap ∘ T.unpackProjection fld

-- TODO: move it somewhere, it should live in different place with group/flatten
showJCursor ∷ J.JCursor → String
showJCursor = case _ of
  J.JCursorTop → "value"
  J.JField i c → i ⊕ show c
  c → show c
