module SlamData.Workspace.Card.Setups.Viz.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.List ((:))
import Data.List as L
import Data.Lens ((^.), (?~), (^?), _Just)
import Data.Set as Set
import Data.Map as Map
import Data.StrMap as SM

import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimMap.Component.State (Package, interpret, axesComposer)
import SlamData.Workspace.Card.Setups.DimMap.DSL (field, optional, addSource, isFilteredBy, isActiveWhen)
import SlamData.Workspace.Card.Setups.Package.Projection as PP

type State =
  { vizType ∷ VT.VizType
  , vizTypePickerExpanded ∷ Boolean
  , axes ∷ Maybe Ax.Axes
  , dimMaps ∷ Map.Map VT.VizType T.DimensionMap
  }


initialState ∷ State
initialState =
  { vizType: VT.Chart VT.Pie
  , vizTypePickerExpanded: false
  , axes: Nothing
  , dimMaps: Map.fromFoldable $ map (_ × T.emptyDimMap) VT.all
  }

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
    , cursorMap: \dm axes →
       let
         lattify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
         lattify = T.unpackProjection PP._lat ?~ axes.value

         longify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
         longify =
           T.unpackProjection PP._lng ?~ axesComposer.filter PP._lat dm axes.value

         latLngFilter ∷ Set.Set JCursor → Set.Set JCursor
         latLngFilter =
           axesComposer.guard PP._lat dm
           ∘ axesComposer.guard PP._lng dm
           ∘ axesComposer.filter PP._lat dm
           ∘ axesComposer.filter PP._lng dm

         seriify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
         seriify =
           T.unpackProjection PP._series
           ?~ (latLngFilter $ axes.category <> axes.date <> axes.datetime <> axes.time)

         sizify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
         sizify =
           T.unpackProjection PP._size ?~ latLngFilter axes.value

         prepared ∷ SM.StrMap (Set.Set JCursor)
         prepared =
           SM.empty # lattify ∘ longify ∘ seriify ∘ sizify

         flds ∷ L.List T.Projection
         flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims

         foldFn ∷ SM.StrMap (Set.Set JCursor) → T.Projection → SM.StrMap (Set.Set JCursor)
         foldFn acc fld =
           let
             indices ∷ L.List Int
             indices = maybe L.Nil (L.range zero ∘ flip sub one) $ PP.mbDimIx fld

             cursorSet ∷ Set.Set JCursor
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
    , cursorMap: \dm axes →
       let
         seried ∷ SM.StrMap (Set.Set JCursor)
         seried =
           SM.empty
           # (T.unpackProjection PP._series)
           ?~ (axes.category <> axes.time <> axes.date <> axes.datetime)

         flds ∷ L.List T.Projection
         flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims

         foldFn ∷ SM.StrMap (Set.Set JCursor) → T.Projection → SM.StrMap (Set.Set JCursor)
         foldFn acc fld =
           let
             indices ∷ L.List Int
             indices = maybe L.Nil (flip sub one ⋙ L.range zero) $ PP.mbDimIx fld

             cursorSet ∷ Set.Set JCursor
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
    ]
  mapFromFree = Map.fromFoldable
    [ VT.Input VT.Text × do
    _ ←
      field PP._value
      >>= addSource _.value
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime
    pure unit

  , VT.Input VT.Numeric × do
    _ ←
      field PP._value
      >>= addSource _.value
    pure unit

  , VT.Input VT.Date × do
    _ ←
      field PP._value
      >>= addSource _.date
    pure unit

  , VT.Input VT.Time × do
    _ ←
      field PP._value
      >>= addSource _.time
    pure unit

  , VT.Input VT.Datetime × do
    _ ←
      field PP._value
      >>= addSource _.time
    pure unit

  , VT.Select VT.Dropdown × do
    traceAnyA "TODO: free for select"
    pure unit
  , VT.Geo VT.GeoMarker × pure unit
  , VT.Geo VT.GeoHeatmap × do
    lat ←
      field PP._lat
      >>= addSource _.value

    lng ←
      field PP._lng
      >>= addSource _.value
      >>= isFilteredBy lat

    intensity ←
      field PP._intensity
      >>= addSource _.value
      >>= isFilteredBy lat
      >>= isFilteredBy lng
    pure unit

  , VT.Chart VT.Pie × do
    category ←
      field PP._category
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    value ←
      field PP._value
      >>= addSource _.value

    donut ←
      optional PP._donut
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy category
      >>= isActiveWhen category

    parallel ←
      optional PP._parallel
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy category
      >>= isFilteredBy donut
      >>= isActiveWhen category
    pure unit

  , VT.Chart VT.Line × do
    dimension ←
      field PP._dimension
      >>= addSource _.category
      >>= addSource _.value
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    value ←
      field PP._value
      >>= addSource _.value
      >>= isFilteredBy dimension

    secondValue ←
      optional PP._secondValue
      >>= addSource _.value
      >>= isFilteredBy value

    size ←
      optional PP._size
      >>= addSource _.value
      >>= isFilteredBy dimension
      >>= isFilteredBy value
      >>= isFilteredBy secondValue
      >>= isActiveWhen value

    series ←
      optional PP._series
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy dimension
      >>= isActiveWhen dimension
    pure unit

  , VT.Chart VT.Bar × do
    category ←
      field PP._category
      >>= addSource _.category
      >>= addSource _.value
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    value ←
      field PP._value
      >>= addSource _.value
      >>= isFilteredBy category

    stack ←
      optional PP._stack
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy category
      >>= isActiveWhen category

    parallel ←
      optional PP._parallel
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy category
      >>= isFilteredBy stack
      >>= isActiveWhen category
    pure unit

  , VT.Chart VT.Area × do
    dimension ←
      field PP._dimension
      >>= addSource _.time
      >>= addSource _.value
      >>= addSource _.date
      >>= addSource _.datetime
      >>= addSource _.category

    value ←
      field PP._value
      >>= isFilteredBy dimension
      >>= addSource _.value

    series ←
      optional PP._series
      >>= addSource _.time
      >>= addSource _.value
      >>= addSource _.date
      >>= addSource _.datetime
      >>= addSource _.category
      >>= isFilteredBy value
      >>= isFilteredBy dimension
      >>= isActiveWhen dimension

    pure unit

  , VT.Chart VT.Scatter × do
    abscissa ←
      field PP._abscissa
      >>= addSource _.value
    ordinate ←
      field PP._scatterOrdinate
      >>= addSource _.value
      >>= isFilteredBy abscissa
    size ←
      optional PP._scatterSize
      >>= addSource _.value
      >>= isFilteredBy abscissa
      >>= isFilteredBy ordinate
    series ←
      optional PP._series
      >>= addSource _.category
    parallel ←
      optional PP._parallel
      >>= addSource _.category
      >>= isFilteredBy series
    pure unit

  , VT.Chart VT.Radar × do
    category ←
      field PP._category
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    value ←
      field  PP._value
      >>= addSource _.value

    multiple ←
      optional PP._multiple
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy category
      >>= isActiveWhen category

    parallel ←
      optional PP._parallel
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy category
      >>= isFilteredBy multiple
      >>= isActiveWhen category

    pure unit

  , VT.Chart VT.Funnel × do
    category ←
      field PP._category
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    value ←
      field PP._value
      >>= addSource _.value

    series ←
      optional PP._series
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime
      >>= isFilteredBy category
      >>= isActiveWhen category

    pure unit

  , VT.Chart VT.Graph × do
    source ←
      field PP._source
      >>= addSource _.category

    target ←
      field PP._target
      >>= addSource _.category
      >>= isFilteredBy source

    size ←
      optional PP._size
      >>= addSource _.value

    color ←
      optional PP._color
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy source
      >>= isFilteredBy target

    pure unit

  , VT.Chart VT.Heatmap × do
    abscissa ←
      field PP._abscissa
      >>= addSource _.category
      >>= addSource _.value
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    ordinate ←
      field PP._ordinate
      >>= addSource _.category
      >>= addSource _.value
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime
      >>= isFilteredBy abscissa

    value ←
      field PP._value
      >>= addSource _.value
      >>= isFilteredBy abscissa
      >>= isFilteredBy ordinate

    series ←
      optional PP._series
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy abscissa
      >>= isFilteredBy ordinate
      >>= isActiveWhen abscissa
      >>= isActiveWhen ordinate

    pure unit

  , VT.Chart VT.Sankey × do
    source ←
      field PP._source
      >>= addSource _.category

    target ←
      field PP._target
      >>= addSource _.category
      >>= isFilteredBy source

    value ←
      field PP._value
      >>= addSource _.value

    pure unit

  , VT.Chart VT.Gauge × do
    value ←
      field PP._value
      >>= addSource _.value

    parallel ←
      optional PP._parallel
      >>= addSource _.category
      >>= addSource _.time
      >>= isActiveWhen value

    multiple ←
      optional PP._multiple
      >>= addSource _.category
      >>= addSource _.time
      >>= isActiveWhen value
      >>= isFilteredBy parallel

    pure unit

  , VT.Chart VT.Boxplot × do
    dimension ←
      field PP._dimension
      >>= addSource _.category
      >>= addSource _.value
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    value ←
      field PP._flatValue
      >>= addSource _.value
      >>= isFilteredBy dimension

    series ←
      optional PP._series
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy dimension
      >>= isActiveWhen dimension


    parallel ←
      optional PP._parallel
      >>= addSource _.category
      >>= addSource _.time
      >>= isFilteredBy dimension
      >>= isFilteredBy series
      >>= isActiveWhen dimension
    pure unit

  , VT.Chart VT.PunchCard × do
    abscissa ←
      field PP._abscissa
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    ordinate ←
      field PP._ordinate
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    value ←
      field PP._value
      >>= addSource _.value
    pure unit

  , VT.Chart VT.Candlestick × do
    dimension ←
      field PP._dimension
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime

    open ←
      field PP._open
      >>= addSource _.value

    close ←
      field PP._close
      >>= addSource _.value
      >>= isFilteredBy open

    high ←
      field PP._high
      >>= addSource _.value
      >>= isFilteredBy open
      >>= isFilteredBy close

    low ←
      field PP._low
      >>= addSource _.value
      >>= isFilteredBy open
      >>= isFilteredBy close
      >>= isFilteredBy high

    parallel ←
      optional PP._parallel
      >>= addSource _.category
      >>= addSource _.time
      >>= addSource _.date
      >>= addSource _.datetime
      >>= isFilteredBy dimension
      >>= isActiveWhen dimension


    pure unit

  , VT.Chart VT.Parallel ×
    pure unit

  , VT.Metric × do
    _ ←
      field PP._value
      >>= addSource _.value
      >>= addSource _.category
    pure unit
  ]
