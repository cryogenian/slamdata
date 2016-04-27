module SlamData.Header.Gripper.Component where

import SlamData.Prelude


import DOM.HTML (window)
import DOM.HTML.Window as Win
import DOM.HTML.Types as Ht
import DOM.Event.EventTarget as Etr
import DOM.Event.EventTypes as Etp
import DOM.Node.ParentNode as Pn
import Data.Nullable as N
import DOM.Node.Types as Dt

import CSS.Geometry (marginTop)
import CSS.Size (px)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, (?), keyframesFromTo)
import CSS.Animation (animation, iterationCount, normalAnimationDirection, forwards)
import CSS.Time (sec)
import CSS.Transition (easeOut)

import Halogen as H
import Halogen.Component.Utils as Hu
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)

data Query a
  = Init a
  | StartDragging Number a
  | StopDragging a
  | ChangePosition Number a
  | Animated a

data Direction = Up | Down

data State
  = Closed
  | Opened
  | Dragging Direction Number Number
  | Opening Number
  | Closing Number

initialState ∷ State
initialState = Closed

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ String → H.Component State Query Slam
comp querySelector = H.lifecycleComponent
  { render: render querySelector
  , eval: eval querySelector
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

render ∷ String → State → HTML
render sel state =
  HH.div
    [ HP.classes
        [ HH.className "header-gripper"
        ]
    , HE.onMouseDown \evt →
        HEH.preventDefault $> (H.action $ StartDragging evt.clientY)
    ]
    [ HH.div
        [ ARIA.label $ label state
        ]
        [ glyph B.glyphiconResizeVertical
        , CSS.stylesheet $ renderStyles sel state
        ]
    ]
  where
  label ∷ State → String
  label Closed = "Show header"
  label (Opening _) = "Show header"
  label (Dragging Down _ _) = "Show header"
  label _ = "Hide header"


maxMargin ∷ Number
maxMargin = 70.0

animationDuration ∷ Number
animationDuration = 0.5

renderStyles ∷ String → State → CSS
renderStyles sel Closed = do
  (fromString sel) ? (marginTop $ px (-maxMargin))
renderStyles sel Opened = do
  (fromString sel) ? (marginTop $ px zero)
renderStyles sel (Opening startPos) = do
  let
    marginTo = zero
    marginFrom = startPos - maxMargin
  mkAnimation sel marginFrom marginTo
renderStyles sel (Closing startPos) = do
  let
    marginTo = -maxMargin
    marginFrom = startPos - maxMargin
  mkAnimation sel marginFrom marginTo
renderStyles sel (Dragging _ start current) = do
  (fromString sel) ? do
    marginTop $ px (-maxMargin + current - start)

mkAnimation ∷ String → Number → Number → CSS
mkAnimation sel marginFrom marginTo = do
  let
    marginFromStyle = marginTop $ px marginFrom
    marginToStyle = marginTop $ px marginTo
  (fromString sel) ? do
    keyframesFromTo "header-gripper-margin" marginFromStyle marginToStyle
    animation
      (fromString "header-gripper-margin")
      (sec animationDuration)
      easeOut
      (sec zero)
      (iterationCount one)
      normalAnimationDirection
      forwards

eval ∷ String → (Query ~> DSL)
eval sel (Init next) = do
  doc ←
    H.fromEff
      $ window
      >>= Win.document

  mbNavEl ←
    H.fromEff
      $ Pn.querySelector sel (Ht.htmlDocumentToParentNode doc)
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
eval _ (StartDragging pos next) = do
  astate ← H.get
  case astate of
    Closed → H.set (Dragging Down pos pos)
    Opened → H.set (Dragging Up (pos - maxMargin) pos)
    _ → pure unit
  Hu.forceRerender
  pure next
eval _ (StopDragging next) = do
  astate ← H.get
  case astate of
    Dragging dir s current →
      let
        nextState Down = Opening
        nextState Up = Closing
      in
        H.set (nextState dir $ current - s)
    _ → pure unit
  pure next
eval _ (ChangePosition num next) = do
  astate ← H.get
  let
    toSet s =
      let diff = num - s
      in s + if diff < 0.0 then 0.0 else if diff > maxMargin then maxMargin else diff
    direction oldPos oldDir =
      if num ≡ oldPos then oldDir else if num > oldPos then Down  else Up
  case astate of
    Dragging oldDir s old →
      H.set (Dragging (direction old oldDir) s $ toSet s)
    _ → pure unit
  pure next
eval _ (Animated next) = do
  astate ← H.get
  case astate of
    Opening _ → H.set Opened
    Closing _ → H.set Closed
    _ → pure unit
  pure next
