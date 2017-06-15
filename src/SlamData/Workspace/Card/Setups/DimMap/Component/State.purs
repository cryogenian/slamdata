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

module SlamData.Workspace.Card.Setups.DimMap.Component.State where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Lens (Lens', _Just, lens, (^.), (.~), (?~), (^?), (%~))
import Data.List as L
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimMap.Defaults as DMD
import SlamData.Workspace.Card.Setups.Transform as Tr
import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.Setups.DimMap.DSL as DSL

import Utils (showPrettyJCursor)

type Package = DSL.Package (Set.Set J.JCursor) ()

interpret ∷ DSL.DimensionM Unit → Package
interpret = DSL.interpret axesComposer

type State =
  { axes ∷ Ax.Axes
  , dimMap ∷ T.DimensionMap
  , selected ∷ Maybe (T.Projection ⊹ T.Projection)
  , package ∷ Package
  }

initialState ∷ ∀ a. a → State
initialState _ =
  { axes: Ax.initialAxes
  , dimMap: T.emptyDimMap
  , selected: Nothing
  , package: interpret $ pure unit
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

projectionCursors ∷ T.Projection → State → Set.Set J.JCursor
projectionCursors prj state =
  fromMaybe Set.empty
    $ state.package.cursorMap state.dimMap state.axes
    ^. T.unpackProjection prj

selectedCursors ∷ State → L.List J.JCursor
selectedCursors state = case state.selected of
  Just (Left lns) → L.fromFoldable $ projectionCursors lns state
  _ → L.Nil

isDisabled ∷ T.Projection → State → Boolean
isDisabled prj state =
  Set.isEmpty $ projectionCursors prj state

isConfigurable ∷ T.Projection → State → Boolean
isConfigurable prj state
  | DMD.isFlat prj = false
  | otherwise =
    let
      axis = state.dimMap ^? T.unpackProjection prj ∘ _Just ∘ D._value ∘ D._projection
    in maybe false (eq Ax.Measure) $ Ax.axisType <$> axis <*> pure state.axes

getTransform ∷ T.Projection → State → Maybe Tr.Transform
getTransform tp state =
  state.dimMap ^? T.unpackProjection tp ∘ _Just ∘ D._value ∘ D._transform ∘ _Just

transforms ∷ State → Array Tr.Transform
transforms _ = Tr.aggregationTransforms

setValue ∷ T.Projection → J.JCursor → State → State
setValue fld cursor state
  | (Just cursor) ≡
    (state ^? _dimMap ∘ T.unpackProjection fld ∘ _Just ∘ D._value ∘ D._projection) =
      state
  | otherwise =
      state
        # (_dimMap ∘ T.unpackProjection fld ?~ wrapFn cursor)
        ∘ (_dimMap %~ deselectJCursor cursor)
  where
  deselectJCursor ∷ J.JCursor → T.DimensionMap → T.DimensionMap
  deselectJCursor jc dimMap =
    SM.fromFoldable
    $ L.filter (\(k × d) → Just jc ≠ (d ^? D._value ∘ D._projection))
    $ SM.toUnfoldable dimMap

  wrapFn = (DMD.getDefaults fld).dimension

showValue ∷ T.Projection → Maybe J.JCursor → String
showValue fld c = do
  fromMaybe (DMD.getDefaults fld).value $ map showPrettyJCursor c

chooseLabel ∷ T.Projection → String
chooseLabel = _.select ∘ DMD.getDefaults

showDefaultLabel ∷ T.Projection → Maybe J.JCursor → String
showDefaultLabel = const ∘ _.label ∘ DMD.getDefaults

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

labelless ∷ T.Projection → Boolean
labelless fld = (DMD.getDefaults fld).labelless
