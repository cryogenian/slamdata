{-
Copyright 2017 SlamData, Inc.
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
import Data.Lens (Lens', _Just, lens, (^.), (.~), (?~), (^?), (%~))
import Data.Lens.At (at)
import Data.List as L
import Data.Set as Set

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.DimensionMap.Package as DP
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as Pr
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as Tr

import Utils (showPrettyJCursor)

type State =
  { axes ∷ Ax.Axes
  , dimMap ∷ Pr.DimensionMap
  , selected ∷ Maybe (Pr.Projection ⊹ Pr.Projection)
  , package ∷ DP.Package
  }

initialState ∷ ∀ a. a → State
initialState _ =
  { axes: Ax.initialAxes
  , dimMap: Pr.empty
  , selected: Nothing
  , package: DP.interpret $ pure unit
  }

_axes ∷ ∀ a r. Lens' { axes ∷ a | r } a
_axes = lens _.axes _{ axes = _ }

_selected ∷ ∀ a r. Lens' { selected ∷ a | r } a
_selected = lens _.selected _{ selected = _ }

_dimMap ∷ ∀ a r. Lens' { dimMap ∷ a | r } a
_dimMap = lens _.dimMap _{ dimMap = _ }


projectionCursors ∷ Pr.Projection → State → Set.Set J.JCursor
projectionCursors prj state =
  fromMaybe Set.empty
    $ Pr.lookup prj
    $ state.package.cursorMap state.dimMap state.axes

selectedCursors ∷ State → L.List J.JCursor
selectedCursors state = case state.selected of
  Just (Left lns) → L.fromFoldable $ projectionCursors lns state
  _ → L.Nil

isDisabled ∷ Pr.Projection → State → Boolean
isDisabled prj state =
  Set.isEmpty $ projectionCursors prj state

isConfigurable ∷ Pr.Projection → State → Boolean
isConfigurable prj state
  | Pr.isFlat prj = false
  | otherwise =
    let
      axis = do
        jc ← Pr.lookup prj state.dimMap
        jc ^? D._value ∘ D._projection
    in maybe false (eq Ax.Measure) $ Ax.axisType <$> axis <*> pure state.axes

getTransform ∷ Pr.Projection → State → Maybe Tr.Transform
getTransform tp state = do
  jc ← Pr.lookup tp state.dimMap
  jc ^? D._value ∘ D._transform ∘ _Just

transforms ∷ State → Array Tr.Transform
transforms _ = Tr.aggregationTransforms

setValue ∷ Pr.Projection → J.JCursor → State → State
setValue fld cursor state
  | (Just cursor) ≡
    (state ^? _dimMap ∘ at fld ∘ _Just ∘ D._value ∘ D._projection) =
      state
  | otherwise =
      state
        # (_dimMap ∘ at fld ?~ wrapFn cursor)
        ∘ (_dimMap %~ deselectJCursor cursor)
  where
  deselectJCursor jc =
    Pr.filter (\d → Just jc ≠ d ^? D._value ∘ D._projection)
  wrapFn = Pr.getDimension fld

showValue ∷ Pr.Projection → Maybe J.JCursor → String
showValue fld c = do
  fromMaybe (Pr.getValue fld) $ map showPrettyJCursor c

chooseLabel ∷ Pr.Projection → String
chooseLabel = Pr.getSelect

showDefaultLabel ∷ Pr.Projection → Maybe J.JCursor → String
showDefaultLabel = const ∘ Pr.getLabel

setTransform ∷ Pr.Projection → Maybe Tr.Transform → State → State
setTransform fld t =
  _dimMap ∘ at fld ∘ _Just ∘ D._value ∘ D._transform .~ t

setLabel ∷ Pr.Projection → String → State → State
setLabel fld str = _dimMap ∘ at fld ∘ _Just ∘ D._category ?~ D.Static str

clear ∷ Pr.Projection → State → State
clear fld = _dimMap ∘ at fld .~ Nothing

select ∷ Pr.Projection → State → State
select fld = _selected .~ (Just $ Left fld)

configure ∷ Pr.Projection → State → State
configure fld = _selected .~ (Just $ Right fld)

deselect ∷ State → State
deselect = _selected .~ Nothing

getSelected ∷ Pr.Projection → State → Maybe D.LabeledJCursor
getSelected fld state = state ^. _dimMap ∘ at fld

labelless ∷ Pr.Projection → Boolean
labelless = Pr.isLabelless
