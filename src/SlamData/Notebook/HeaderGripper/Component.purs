module SlamData.Notebook.HeaderGripper.Component where

import SlamData.Prelude

import Data.Lens (LensP, lens, (.~), (?~))

import DOM.HTML (window)
import DOM.HTML.Window as Win
import DOM.HTML.Types as Ht
import DOM.Event.EventTarget as Etr
import DOM.Event.EventTypes as Etp

import CSS.Geometry (marginTop, paddingTop)
import CSS.Size (px)
import CSS.String (fromString)
import CSS.Stylesheet ((?))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.CSS as CSS

import SlamData.Effects (Slam)

data Query a
  = Init a
  | StartDragging Number a
  | StopDragging a
  | ShowAll a
  | ChangePosition Number a

type State =
  { draggedPx ∷ Number
  , dragStartedAt ∷ Maybe Number
  , expanded ∷ Boolean
  }

_draggedPx ∷ ∀ a r. LensP {draggedPx ∷ a|r} a
_draggedPx = lens (_.draggedPx) (_{draggedPx = _})

_dragStartedAt ∷ ∀ a r. LensP {dragStartedAt ∷ a|r} a
_dragStartedAt = lens (_.dragStartedAt) (_{dragStartedAt = _})

_expanded ∷ ∀ a r. LensP {expanded ∷ a|r} a
_expanded = lens (_.expanded) (_{expanded = _})

initialState ∷ State
initialState =
  { draggedPx: 0.0
  , dragStartedAt: Nothing
  , expanded: false
  }

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ H.Component State Query Slam
comp = H.lifecycleComponent
  { render
  , eval
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

render ∷ State → HTML
render {draggedPx} =
  HH.div
    [ HP.classes
        [ HH.className "header-gripper"
        ]
    , HE.onClick (HE.input_ ShowAll)
    , HE.onMouseDown \evt →
        HEH.preventDefault $> (H.action $ StartDragging evt.clientY)
    ]
    [ HH.div
        [ ]
        [ HH.text "SlamData"
        , CSS.stylesheet do
            (fromString "nav") ? do
              marginTop $ px (-70.0 + draggedPx)
            (fromString ".sd-notebook") ? do
              paddingTop $ px (43.0 + draggedPx)
            pure unit
        ]
    ]

eval ∷ Query ~> DSL
eval (Init next) = do
  docTarget ←
    H.fromEff
      $ window
      >>= Win.document
      <#> Ht.htmlDocumentToEventTarget

  let
    evntify ∷ ∀ a. a → { clientY ∷ Number }
    evntify = Unsafe.Coerce.unsafeCoerce

    attachMouseUp f =
      Etr.addEventListener Etp.mouseup (Etr.eventListener f) false docTarget
    attachMouseMove f =
      Etr.addEventListener Etp.mousemove (Etr.eventListener f) false docTarget
    handleMouseUp e =
      pure $ H.action $ StopDragging
    handleMouseMove e =
      pure $ H.action $ ChangePosition (evntify e).clientY


  H.subscribe $ H.eventSource attachMouseUp handleMouseUp
  H.subscribe $ H.eventSource attachMouseMove handleMouseMove
  pure next
eval (StartDragging pos next) = do
  H.modify (_dragStartedAt ?~ pos)
  pure next
eval (StopDragging next) = do
  H.modify
    $ (_draggedPx .~ 0.0)
    ∘ (_dragStartedAt .~ Nothing)
  pure next
eval (ShowAll next) = do
  drPx ← H.gets _.draggedPx
  let toSet = if drPx > 0.0 then 0.0 else 70.0
  H.modify
    $ (_draggedPx .~ toSet)
    ∘ (_dragStartedAt .~ Nothing)
  pure next
eval (ChangePosition num next) = do
  mbStartedAt ← H.gets _.dragStartedAt
  for_ mbStartedAt \startedAt →
    let
      diff = num - startedAt
      toSet =
        if diff < 0.0 then 0.0 else if diff > 70.0 then 70.0 else diff
    in
      H.modify (_draggedPx .~ toSet)
  pure next
