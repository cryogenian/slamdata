module SlamData.Notebook.HeaderGripper.Component where

import SlamData.Prelude

import Data.Lens (LensP, lens, (.~), (?~))

import DOM.HTML (window)
import DOM.HTML.Window as Win
import DOM.HTML.Types as Ht
import DOM.Event.EventTarget as Etr
import DOM.Event.EventTypes as Etp
import DOM.Node.ParentNode as Pn
import Data.Nullable as N
import DOM.Node.Types as Dt
import DOM.Event.Types as Et

import CSS.Geometry (marginTop, paddingTop)
import CSS.Size (px)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, (?), keyframesFromTo)
import CSS.Animation (animation, iterationCount, normalAnimationDirection, forwards)
import CSS.Time (sec)
import CSS.Transition (easeOut)

import Halogen as H
import Halogen.Component.Utils as Hu
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
  | ChangePosition Number a
  | Animated a

data Direction = Up | Down

data AnimationState
  = Closed
  | Opened
  | Dragging Direction Number Number
  | Opening Number
  | Closing Number

type State =
  { draggedPx ∷ Number
  , dragStartedAt ∷ Maybe Number
  , expanded ∷ Boolean
  , animationState ∷ AnimationState
  }

_draggedPx ∷ ∀ a r. LensP {draggedPx ∷ a|r} a
_draggedPx = lens (_.draggedPx) (_{draggedPx = _})

_dragStartedAt ∷ ∀ a r. LensP {dragStartedAt ∷ a|r} a
_dragStartedAt = lens (_.dragStartedAt) (_{dragStartedAt = _})

_expanded ∷ ∀ a r. LensP {expanded ∷ a|r} a
_expanded = lens (_.expanded) (_{expanded = _})

_animationState ∷ ∀ a r. LensP {animationState ∷ a|r} a
_animationState = lens (_.animationState) (_{animationState = _})

initialState ∷ State
initialState =
  { draggedPx: 0.0
  , dragStartedAt: Nothing
  , expanded: false
  , animationState: Closed
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
render {draggedPx, animationState} =
  HH.div
    [ HP.classes
        [ HH.className "header-gripper"
        ]
    , HE.onMouseDown \evt →
        HEH.preventDefault $> (H.action $ StartDragging evt.clientY)
    ]
    [ HH.div
        [ ]
        [ HH.img [ HP.src "img/logo.svg" ]
        , HH.text "3.0"
        , CSS.stylesheet $ renderStyles animationState

        ]
    ]


maxMargin ∷ Number
maxMargin = 70.0

animationDuration ∷ Number
animationDuration = 0.5

renderStyles ∷ AnimationState → CSS
renderStyles Closed = do
  (fromString "nav") ? (marginTop $ px (-maxMargin))
  (fromString ".sd-notebook") ? (paddingTop $ px zero)
renderStyles Opened = do
  (fromString "nav") ? (marginTop $ px zero)
  (fromString ".sd-notebook") ? (paddingTop $ px maxMargin)
renderStyles (Opening startPos) = do
  let
    marginTo = zero
    marginFrom = startPos - maxMargin
  mkAnimation marginFrom marginTo
renderStyles (Closing startPos) = do
  let
    marginTo = -maxMargin
    marginFrom = startPos - maxMargin
  mkAnimation marginFrom marginTo
renderStyles (Dragging _ start current) = do
  (fromString "nav") ? do
    marginTop $ px (-maxMargin + current - start)
  (fromString ".sd-notebook") ? do
    paddingTop $ px (current - start)

mkAnimation ∷ Number → Number → CSS
mkAnimation marginFrom marginTo = do
  let
    marginFromStyle = marginTop $ px marginFrom
    marginToStyle = marginTop $ px marginTo
    paddingFromStyle = paddingTop $ px $ maxMargin + marginFrom
    paddingToStyle = paddingTop $ px $ maxMargin + marginTo
  (fromString "nav") ? do
    keyframesFromTo "header-gripper-margin" marginFromStyle marginToStyle
    animation
      (fromString "header-gripper-margin")
      (sec animationDuration)
      easeOut
      (sec zero)
      (iterationCount one)
      normalAnimationDirection
      forwards
  (fromString ".sd-notebook") ? do
    keyframesFromTo "header-gripper-padding" paddingFromStyle paddingToStyle
    animation
      (fromString "header-gripper-padding")
      (sec animationDuration)
      easeOut
      (sec zero)
      (iterationCount one)
      normalAnimationDirection
      forwards

eval ∷ Query ~> DSL
eval (Init next) = do
  doc ←
    H.fromEff
      $ window
      >>= Win.document

  mbNavEl ←
    H.fromEff
      $ Pn.querySelector "nav" (Ht.htmlDocumentToParentNode doc)
      <#> N.toMaybe
  let
    evntify ∷ ∀ a. a → { clientY ∷ Number }
    evntify = Unsafe.Coerce.unsafeCoerce

    docTarget = Ht.htmlDocumentToEventTarget doc

    attachMouseUp f =
      Etr.addEventListener Etp.mouseup (Etr.eventListener f) false docTarget
    attachMouseMove f =
      Etr.addEventListener Etp.mousemove (Etr.eventListener f) false docTarget

    handleMouseUp e =
      pure $ H.action $ StopDragging
    handleMouseMove e = do
      pure $ H.action $ ChangePosition (evntify e).clientY

  H.subscribe $ H.eventSource attachMouseUp handleMouseUp
  H.subscribe $ H.eventSource attachMouseMove handleMouseMove

  for_ mbNavEl \navEl →
    let
      attachAnimationEnd f =
        Etr.addEventListener Etp.animationend (Etr.eventListener f) false
          $ Dt.elementToEventTarget navEl

      handleAnimationEnd e =
        pure $ H.action Animated
    in
      H.subscribe $ H.eventSource attachAnimationEnd handleAnimationEnd
  pure next
eval (StartDragging pos next) = do
  astate ← H.gets _.animationState
  case astate of
    Closed → H.modify (_animationState .~ Dragging Down pos pos)
    Opened → H.modify (_animationState .~ Dragging Up (pos - maxMargin) pos)
    _ → pure unit
  Hu.forceRerender
  pure next
eval (StopDragging next) = do
  astate ← H.gets _.animationState
  case astate of
    Dragging dir s current →
      let
        nextState Down = Opening
        nextState Up = Closing
      in
        H.modify (_animationState .~ (nextState dir $ current - s))
    _ → pure unit
  pure next
eval (ChangePosition num next) = do
  astate ← H.gets _.animationState
  let
    toSet s =
      let diff = num - s
      in s + if diff < 0.0 then 0.0 else if diff > maxMargin then maxMargin else diff
    direction oldPos oldDir =
      if num ≡ oldPos then oldDir else if num > oldPos then Down  else Up
  case astate of
    Dragging oldDir s old →
      H.modify (_animationState .~ (Dragging (direction old oldDir) s $ toSet s))
    _ → pure unit
  pure next
eval (Animated next) = do
  astate ← H.gets _.animationState
  case astate of
    Opening _ → H.modify (_animationState .~ Opened)
    Closing _ → H.modify (_animationState .~ Closed)
    _ → pure unit
  pure next
