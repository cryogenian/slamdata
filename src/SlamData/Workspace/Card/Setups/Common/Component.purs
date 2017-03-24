module SlamData.Workspace.Card.Setups.Common.Component where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Lens ((^.), (.~), (^?), _Just, (%~))
import Data.List as L
import Data.Set as Set

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Common.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Common.State as ST
import SlamData.Workspace.Card.Setups.DimensionPicker.Component.Message as DM
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor as DJ
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode)
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Package as P
import SlamData.Workspace.Card.Setups.Dimension as D

data FieldQuery a
  = Select a
  | Dismiss a
  | Configure a
  | LabelChanged String a
  | HandleDPMessage (DM.Message JCursorNode) a
  | HandleTransformPicker (AS.Message T.Transform) a

data Query a
  = OnField ST.Projection (FieldQuery a)
  | Load (Maybe M.AnyCardModel) a
  | Save M.AnyCardModel (Maybe M.AnyCardModel → a)
  | SetAxes Ax.Axes a

data Message = Update

type State = ST.StateR ()

type HTML = H.ParentHTML Query CS.ChildQuery CS.ChildSlot Slam
type DSL = H.ParentDSL State Query CS.ChildQuery CS.ChildSlot Message Slam

type Package = P.Package M.AnyCardModel (Set.Set J.JCursor)

initialState ∷ ∀ a. a → State
initialState _ =
  { axes: Ax.initialAxes
  , dimMap: ST.emptyDimMap
  , selected: Nothing
  }

projectionCursors ∷ ST.Projection → Package → State →  Set.Set J.JCursor
projectionCursors prj pack state =
  fromMaybe Set.empty
    $ pack.cursorMap state.dimMap state.axes
    ^. ST.unpack prj

selectedCursors ∷ Package → State → L.List J.JCursor
selectedCursors pack state = case state.selected of
  Just (Left lns) → L.fromFoldable $ projectionCursors lns pack state
  _ → L.Nil

isDisabled ∷ ST.Projection → Package → State → Boolean
isDisabled prj pack state =
  Set.isEmpty $ projectionCursors prj pack state

isConfigurable ∷ ST.Projection → Package → State → Boolean
isConfigurable prj pack state =
  let
    axis = state.dimMap ^? ST.unpack prj ∘ _Just ∘ D._value ∘ D._projection
  in maybe false (eq Ax.Measure) $ Ax.axisType <$> axis <*> pure state.axes

component ∷ Package → H.Component HH.HTML Query Unit Message Slam
component package =
  H.parentComponent
    { initialState
    , render: render package
    , eval: eval package
    , receiver: const Nothing
    }

render ∷ Package → State → HTML
render pack state =
  HH.div [ HP.classes [ HH.ClassName "sd-axes-selector" ] ]
  $ ( foldMap (pure ∘ renderButton pack state) pack.allFields )
  ⊕ [ renderSelection pack state ]

renderSelection ∷ Package → State → HTML
renderSelection pack state = case state ^. ST._selected of
  Nothing → HH.text ""
  Just (Right tp) →
    HH.slot' CS.cpTransform unit AS.component
      { options: ST.transforms state
      , selection: Just $ T.Aggregation Ag.Sum
      , title: "Choose transformation"
      , label: T.prettyPrintTransform
      , deselectable: false
      }
      (HE.input \m → OnField tp ∘ HandleTransformPicker m)
  Just (Left pf) →
    let
      conf =
        { title: ST.chooseLabel pf
        , label: DPC.labelNode DJ.showJCursorTip
        , render: DPC.renderNode DJ.showJCursorTip
        , values: DJ.groupJCursors $ selectedCursors pack state
        , isSelectable: DPC.isLeafPath
        }
    in
      HH.slot'
        CS.cpPicker
        unit
        (DPC.picker conf)
        unit
        (HE.input \m → OnField pf ∘ HandleDPMessage m)

renderButton ∷ Package → State → ST.Projection → HTML
renderButton pack state fld =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: isConfigurable fld pack state
    , dimension: sequence $ ST.getSelected fld state
    , showLabel: absurd
    , showDefaultLabel: ST.showDefaultLabel fld
    , showValue: ST.showValue fld
    , onLabelChange: HE.input \l → OnField fld ∘ LabelChanged l
    , onDismiss: HE.input_ $ OnField fld ∘ Dismiss
    , onConfigure: HE.input_ $ OnField fld ∘ Configure
    , onClick: HE.input_ $ OnField fld ∘ Select
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: isDisabled fld pack state
    , dismissable: isJust $ ST.getSelected fld state
    } ]

eval ∷ Package → Query ~> DSL
eval package = case _ of
  Save m k → do
    st ← H.get
    pure $ k $ package.save st.dimMap m
  Load m next → do
    st ← H.get
    H.modify $ ST._dimMap %~ package.load m
    pure next
  SetAxes ax next → do
    H.modify $ ST._axes .~ ax
    pure next
  OnField fld fldQuery → case fldQuery of
    Select next → do
      H.modify $ ST.select fld
      pure next
    Configure next → do
      H.modify $ ST.configure fld
      pure next
    Dismiss next → do
      H.modify $ ST.clear fld
      H.raise Update
      pure next
    LabelChanged str next → do
      H.modify $ ST.setLabel fld str
      H.raise Update
      pure next
    HandleDPMessage m next → case m of
      DPC.Dismiss → do
        H.modify ST.deselect
        pure next
      DPC.Confirm value → do
        H.modify
          $ ( ST.setValue fld $ DJ.flattenJCursors value )
          ∘ ( ST.deselect )
        H.raise Update
        pure next
    HandleTransformPicker msg next → do
      case msg of
        AS.Dismiss →
          H.modify ST.deselect
        AS.Confirm mbt → do
          H.modify
            $ ST.deselect
            ∘ ST.setTransform fld mbt
          H.raise Update
      pure next
