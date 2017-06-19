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
import Data.List ((:))
import Data.Set as Set
import Data.StrMap as SM
import Data.Map as Map

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimMap.Defaults as DMD
import SlamData.Workspace.Card.Setups.Transform as Tr
import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.Setups.DimMap.DSL as DSL
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.Card.Setups.DimMap.DSL (field, optional, addSource, isFilteredBy, isActiveWhen)

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

packages ∷ Map.Map VT.VizType Package
packages =
  Map.union customMap $ map interpret mapFromFree
  where
  geoMarkerPack =
    { allFields: \dm _ →
       let
         flds = map PP._dimIx $ L.range 0 $ 1 + (L.length $ dm ^. PP._dims)
       in
        PP._lat : PP._lng : PP._size : PP._series : flds
    , axesRequirements:
        DSL.noneRequirements { value = 2 }
    , requiredFields: PP._lat : PP._lng : L.Nil
    , cursorMap: \dm axes →
       let
         lattify ∷ SM.StrMap (Set.Set J.JCursor) → SM.StrMap (Set.Set J.JCursor)
         lattify = T.unpackProjection PP._lat ?~ axes.value

         longify ∷ SM.StrMap (Set.Set J.JCursor) → SM.StrMap (Set.Set J.JCursor)
         longify =
           T.unpackProjection PP._lng ?~ axesComposer.filter PP._lat dm axes.value

         latLngFilter ∷ Set.Set J.JCursor → Set.Set J.JCursor
         latLngFilter =
           axesComposer.guard PP._lat dm
           ∘ axesComposer.guard PP._lng dm
           ∘ axesComposer.filter PP._lat dm
           ∘ axesComposer.filter PP._lng dm

         seriify ∷ SM.StrMap (Set.Set J.JCursor) → SM.StrMap (Set.Set J.JCursor)
         seriify =
           T.unpackProjection PP._series
           ?~ (latLngFilter $ axes.category <> axes.date <> axes.datetime <> axes.time)

         sizify ∷ SM.StrMap (Set.Set J.JCursor) → SM.StrMap (Set.Set J.JCursor)
         sizify =
           T.unpackProjection PP._size ?~ latLngFilter axes.value

         prepared ∷ SM.StrMap (Set.Set J.JCursor)
         prepared =
           SM.empty # lattify ∘ longify ∘ seriify ∘ sizify

         flds ∷ L.List T.Projection
         flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims

         foldFn ∷ SM.StrMap (Set.Set J.JCursor) → T.Projection → SM.StrMap (Set.Set J.JCursor)
         foldFn acc fld =
           let
             indices ∷ L.List Int
             indices = maybe L.Nil (L.range zero ∘ flip sub one) $ PP.mbDimIx fld

             cursorSet ∷ Set.Set J.JCursor
             cursorSet =
               Set.fromFoldable
               $ L.catMaybes
               $ indices <#> \ind →
               dm ^? (T.unpackProjection $ PP._dimIx ind) ∘ _Just ∘ D._value ∘ D._projection
           in
            acc # (T.unpackProjection fld) ?~ (latLngFilter $ Set.difference axes.value cursorSet)
       in
        foldl foldFn prepared flds
    }

  parallelPack =
    { allFields: \dm _ →
       let
         flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims
       in
        L.singleton PP._series <> flds <> L.singleton (PP._dimIx $ L.length flds)
    , axesRequirements: DSL.noneRequirements { value = 2 }
    , requiredFields: PP._dimIx 0 : PP._dimIx 1 : L.Nil
    , cursorMap: \dm axes →
       let
         seried ∷ SM.StrMap (Set.Set J.JCursor)
         seried =
           SM.empty
           # (T.unpackProjection PP._series)
           ?~ (axes.category <> axes.time <> axes.date <> axes.datetime)

         flds ∷ L.List T.Projection
         flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims

         foldFn ∷ SM.StrMap (Set.Set J.JCursor) → T.Projection → SM.StrMap (Set.Set J.JCursor)
         foldFn acc fld =
           let
             indices ∷ L.List Int
             indices = maybe L.Nil (flip sub one ⋙ L.range zero) $ PP.mbDimIx fld

             cursorSet ∷ Set.Set J.JCursor
             cursorSet =
               Set.fromFoldable
               $ L.catMaybes
               $ indices <#> \ind →
               dm ^? (T.unpackProjection $ PP._dimIx ind) ∘ _Just ∘ D._value ∘ D._projection
           in
            acc # (T.unpackProjection fld) ?~ (Set.difference axes.value cursorSet)
       in
        foldl foldFn seried flds
    }

  customMap = Map.fromFoldable
    [ VT.Geo VT.GeoMarker × geoMarkerPack
    , VT.Chart VT.Parallel × parallelPack
    ]

  mapFromFree = Map.fromFoldable [
    VT.Input VT.Text × do
    _ ←
      field PP._formValue
      >>= addSource Ax._value
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime
    pure unit

  , VT.Input VT.Numeric × do
    _ ←
      field PP._formValue
      >>= addSource Ax._value
    pure unit

  , VT.Input VT.Date × do
    _ ←
      field PP._formValue
      >>= addSource Ax._date
    pure unit

  , VT.Input VT.Time × do
    _ ←
      field PP._formValue
      >>= addSource Ax._time
    pure unit

  , VT.Input VT.Datetime × do
    _ ←
      field PP._formValue
      >>= addSource Ax._time
    pure unit

  , VT.Select VT.Dropdown × do
    value ←
      field PP._formValue
      >>= addSource Ax._value
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    label ←
      field PP._formLabel
      >>= addSource Ax._category
      >>= isFilteredBy value

    selected ←
      field PP._formSelected
      >>= addSource Ax._category
      >>= addSource Ax._value
      >>= addSource Ax._date
      >>= addSource Ax._time
      >>= addSource Ax._datetime
      >>= isFilteredBy value
      >>= isFilteredBy label
      >>= isActiveWhen value

    pure unit

  , VT.Select VT.Radio × do
    value ←
      field PP._formValue
      >>= addSource Ax._value
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    label ←
      field PP._formLabel
      >>= addSource Ax._category
      >>= isFilteredBy value

    selected ←
      field PP._formSelected
      >>= addSource Ax._category
      >>= addSource Ax._value
      >>= addSource Ax._date
      >>= addSource Ax._time
      >>= addSource Ax._datetime
      >>= isFilteredBy value
      >>= isFilteredBy label
      >>= isActiveWhen value

    pure unit


  , VT.Select VT.Checkbox × do
    value ←
      field PP._formValue
      >>= addSource Ax._value
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    label ←
      field PP._formLabel
      >>= addSource Ax._category
      >>= isFilteredBy value

    selected ←
      field PP._formSelected
      >>= addSource Ax._category
      >>= addSource Ax._value
      >>= addSource Ax._date
      >>= addSource Ax._time
      >>= addSource Ax._datetime
      >>= isFilteredBy value
      >>= isFilteredBy label
      >>= isActiveWhen value

    pure unit

  , VT.Geo VT.GeoHeatmap × do
    lat ←
      field PP._lat
      >>= addSource Ax._value

    lng ←
      field PP._lng
      >>= addSource Ax._value
      >>= isFilteredBy lat

    intensity ←
      field PP._intensity
      >>= addSource Ax._value
      >>= isFilteredBy lat
      >>= isFilteredBy lng
    pure unit

  , VT.Chart VT.Pie × do
    category ←
      field PP._category
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    value ←
      field PP._value
      >>= addSource Ax._value

    donut ←
      optional PP._donut
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy category
      >>= isActiveWhen category

    parallel ←
      optional PP._parallel
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy category
      >>= isFilteredBy donut
      >>= isActiveWhen category
    pure unit

 , VT.Chart VT.Line × do
    dimension ←
      field PP._dimension
      >>= addSource Ax._category
      >>= addSource Ax._value
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    value ←
      field PP._value
      >>= addSource Ax._value
      >>= isFilteredBy dimension

    secondValue ←
      optional PP._secondValue
      >>= addSource Ax._value
      >>= isFilteredBy value

    size ←
      optional PP._size
      >>= addSource Ax._value
      >>= isFilteredBy dimension
      >>= isFilteredBy value
      >>= isFilteredBy secondValue
      >>= isActiveWhen value

    series ←
      optional PP._series
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy dimension
      >>= isActiveWhen dimension
    pure unit

  , VT.Chart VT.Bar × do
    category ←
      field PP._category
      >>= addSource Ax._category
      >>= addSource Ax._value
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    value ←
      field PP._value
      >>= addSource Ax._value
      >>= isFilteredBy category

    stack ←
      optional PP._stack
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy category
      >>= isActiveWhen category

    parallel ←
      optional PP._parallel
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy category
      >>= isFilteredBy stack
      >>= isActiveWhen category
    pure unit

  , VT.Chart VT.Area × do
    dimension ←
      field PP._dimension
      >>= addSource Ax._time
      >>= addSource Ax._value
      >>= addSource Ax._date
      >>= addSource Ax._datetime
      >>= addSource Ax._category

    value ←
      field PP._value
      >>= isFilteredBy dimension
      >>= addSource Ax._value

    series ←
      optional PP._series
      >>= addSource Ax._time
      >>= addSource Ax._value
      >>= addSource Ax._date
      >>= addSource Ax._datetime
      >>= addSource Ax._category
      >>= isFilteredBy value
      >>= isFilteredBy dimension
      >>= isActiveWhen dimension

    pure unit

  , VT.Chart VT.Scatter × do
    abscissa ←
      field PP._abscissa
      >>= addSource Ax._value
    ordinate ←
      field PP._scatterOrdinate
      >>= addSource Ax._value
      >>= isFilteredBy abscissa
    size ←
      optional PP._scatterSize
      >>= addSource Ax._value
      >>= isFilteredBy abscissa
      >>= isFilteredBy ordinate
    series ←
      optional PP._series
      >>= addSource Ax._category
    parallel ←
      optional PP._parallel
      >>= addSource Ax._category
      >>= isFilteredBy series
    pure unit

  , VT.Chart VT.Radar × do
    category ←
      field PP._category
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    value ←
      field  PP._value
      >>= addSource Ax._value

    multiple ←
      optional PP._multiple
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy category
      >>= isActiveWhen category

    parallel ←
      optional PP._parallel
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy category
      >>= isFilteredBy multiple
      >>= isActiveWhen category

    pure unit

  , VT.Chart VT.Funnel × do
    category ←
      field PP._category
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    value ←
      field PP._value
      >>= addSource Ax._value

    series ←
      optional PP._series
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime
      >>= isFilteredBy category
      >>= isActiveWhen category

    pure unit

  , VT.Chart VT.Graph × do
    source ←
      field PP._source
      >>= addSource Ax._category

    target ←
      field PP._target
      >>= addSource Ax._category
      >>= isFilteredBy source

    size ←
      optional PP._size
      >>= addSource Ax._value

    color ←
      optional PP._color
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy source
      >>= isFilteredBy target

    pure unit

  , VT.Chart VT.Heatmap × do
    abscissa ←
      field PP._abscissa
      >>= addSource Ax._category
      >>= addSource Ax._value
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    ordinate ←
      field PP._ordinate
      >>= addSource Ax._category
      >>= addSource Ax._value
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime
      >>= isFilteredBy abscissa

    value ←
      field PP._value
      >>= addSource Ax._value
      >>= isFilteredBy abscissa
      >>= isFilteredBy ordinate

    series ←
      optional PP._series
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy abscissa
      >>= isFilteredBy ordinate
      >>= isActiveWhen abscissa
      >>= isActiveWhen ordinate

    pure unit

  , VT.Chart VT.Sankey × do
    source ←
      field PP._source
      >>= addSource Ax._category

    target ←
      field PP._target
      >>= addSource Ax._category
      >>= isFilteredBy source

    value ←
      field PP._value
      >>= addSource Ax._value

    pure unit

  , VT.Chart VT.Gauge × do
    value ←
      field PP._value
      >>= addSource Ax._value

    parallel ←
      optional PP._parallel
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isActiveWhen value

    multiple ←
      optional PP._multiple
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isActiveWhen value
      >>= isFilteredBy parallel

    pure unit

  , VT.Chart VT.Boxplot × do
    dimension ←
      field PP._dimension
      >>= addSource Ax._category
      >>= addSource Ax._value
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    value ←
      field PP._flatValue
      >>= addSource Ax._value
      >>= isFilteredBy dimension

    series ←
      optional PP._series
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy dimension
      >>= isActiveWhen dimension


    parallel ←
      optional PP._parallel
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= isFilteredBy dimension
      >>= isFilteredBy series
      >>= isActiveWhen dimension
    pure unit

  , VT.Chart VT.PunchCard × do
    abscissa ←
      field PP._abscissa
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    ordinate ←
      field PP._ordinate
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    value ←
      field PP._value
      >>= addSource Ax._value
    pure unit

  , VT.Chart VT.Candlestick × do
    dimension ←
      field PP._dimension
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime

    open ←
      field PP._open
      >>= addSource Ax._value

    close ←
      field PP._close
      >>= addSource Ax._value
      >>= isFilteredBy open

    high ←
      field PP._high
      >>= addSource Ax._value
      >>= isFilteredBy open
      >>= isFilteredBy close

    low ←
      field PP._low
      >>= addSource Ax._value
      >>= isFilteredBy open
      >>= isFilteredBy close
      >>= isFilteredBy high

    parallel ←
      optional PP._parallel
      >>= addSource Ax._category
      >>= addSource Ax._time
      >>= addSource Ax._date
      >>= addSource Ax._datetime
      >>= isFilteredBy dimension
      >>= isActiveWhen dimension


    pure unit

  , VT.Metric × do
    _ ←
      field PP._value
      >>= addSource Ax._value
      >>= addSource Ax._category
    pure unit
  ]
