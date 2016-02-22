module SlamData.Dialog.Share.RotarySelector.Component
  ( comp
  , module SlamData.Dialog.Share.RotarySelector.Component.State
  , module SlamData.Dialog.Share.RotarySelector.Component.Query
  ) where

import Prelude hiding (top)

import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random, RANDOM())
import Control.Monad.Trans (lift)
import Control.Monad.Maybe.Trans as Mt
import Control.UI.Browser as Br
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling as SCR

import Data.Array as Arr
import Data.Either as E
import Data.Functor (($>))
import Data.Functor.Eff (liftEff)
import Data.Lens (LensP(), lens, (%~), (.~), (?~))
import Data.Lens as  L
import Data.Maybe as M
import Data.NonEmpty ((:|))
import Data.Foldable as F
import Data.StrMap as Sm
import Data.Int as Int
import Data.String.Regex as Rgx
import Data.NonEmpty as Ne

import CSS.Geometry (height, width, left, marginLeft, padding, top)
import CSS.Size (px, pct)
import CSS.Stylesheet (CSS(), keyframesFromTo, (?), key)
import CSS.Animation (animation, iterationCount, normalAnimationDirection, forwards)
import CSS.Time (sec)
import CSS.Transition (easeOut)
import CSS.String (fromString)
import CSS.Render as Cr
import CSS.Selector
  (Selector(..), Predicate(AttrVal), Path(..), Refinement(..), (**), (##))
import CSS.Display (position, relative)
import CSS.Border (border, solid)
import CSS.Color (black)
import CSS.Overflow (overflow, hidden)
import CSS.Display (display, inlineBlock)
import CSS.TextAlign (textAlign, center)

import Halogen hiding (Prop())
import Halogen.HTML.Core (Prop(..), attrName, className, ClassName())
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties.Indexed (IProp())
import Halogen.Query.EventSource (EventSource(..))

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Window as Win
import DOM.HTML.Types as Ht
import DOM.Event.EventTarget as Etr
import DOM.Event.EventTypes as Etp

import SlamData.Effects (Slam())
import SlamData.Form.Select
import SlamData.Dialog.Share.RotarySelector.Component.State
import SlamData.Dialog.Share.RotarySelector.Component.Query

import Utils.Random (genKey, randomString)
import Utils.DOM (getComputedStyle, getClientRects)
import Utils.Array (repeat, shift)
import Utils.NonEmpty (liftNonEmpty)

type RotarySelectorDSL = ComponentDSL State Query Slam

wrapperClass :: ClassName
wrapperClass = className "rotary-selector-wrapper"

draggedClass :: ClassName
draggedClass = className "rotary-selector-dragged"

itemClass :: ClassName
itemClass = className "rotary-selector-item"

dataRotaryKey :: forall i r. String -> IProp r i
dataRotaryKey = Unsafe.Coerce.unsafeCoerce nonIndexed
  where
  nonIndexed :: String -> Prop i
  nonIndexed = Attr M.Nothing (attrName "data-rotarykey")


comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.div wrapperAttrs
    [ H.div [ E.onMouseDown (\evt -> E.preventDefault
                                       $> (action $ StartDragging evt.clientX))
            , P.initializer (\el -> action $ Init el)
            ]
      ( stls <> content)
    ]
  where
  wrapperAttrs =
    [ P.classes [ wrapperClass ] ]
    <> F.foldMap (dataRotaryKey >>> Arr.singleton) state.key

  content :: Array (ComponentHTML Query)
  content =
    Ne.oneOf $ map itemRender state.displayedItems

  itemRender :: String -> ComponentHTML Query
  itemRender s =
    H.div [ P.classes [ itemClass ] ] [ H.text s ]

  stls :: Array (ComponentHTML Query)
  stls =
    F.foldMap (Arr.singleton <<< CSS.stylesheet <<< mkStylesheet) state.key

  mkStylesheet :: String -> CSS
  mkStylesheet k = do
    state.constStyles
    (draggedSelector k) ? state.styles



wrapperSelector :: String -> Selector
wrapperSelector k =
  (fromString ".rotary-selector-wrapper")
  ## (Refinement [ AttrVal "data-rotarykey" k ])

draggedSelector :: String -> Selector
draggedSelector k =
  (wrapperSelector k)
  ** (fromString ".rotary-selector-dragged")

itemSelector :: String -> Selector
itemSelector k =
  (draggedSelector k)
  ** (fromString ".rotary-selector-item")

getCurrentX
  :: RotarySelectorDSL Number
getCurrentX =
  M.fromMaybe zero <$> Mt.runMaybeT do
    el <- Mt.MaybeT $ gets _.element
    st <- liftEff $ getComputedStyle el
    Mt.MaybeT
      $ pure
      $ Sm.lookup "marginLeft" st
      <#> Global.readFloat

getElementOffset
  :: RotarySelectorDSL Number
getElementOffset =
  M.fromMaybe zero <$> Mt.runMaybeT do
    el <- Mt.MaybeT $ gets _.element
    rlst <- liftEff $ getClientRects el
    hd <- Mt.MaybeT $ pure $ Arr.head rlst
    pure hd.left

setDisplayedItems :: Ne.NonEmpty Array String -> RotarySelectorDSL Unit
setDisplayedItems arr = do
  screenWidth <- liftEff $ Br.getScreen <#> _.width
  let
    times =
      Int.ceil
      $ (Int.toNumber screenWidth / 200.0 * 2.0)
      / (Int.toNumber (Arr.length $ Ne.oneOf arr))
  modify $ _displayedItems .~ liftNonEmpty (repeat times) arr


eval :: Natural Query RotarySelectorDSL
eval (Init el next) = do
  modify $ _element ?~ el
  key <- genKey
  modify $ _key ?~ key
  state <- get
  setDisplayedItems state.items
  docTarget <-
    liftEff
    $ window
    >>= Win.document
    <#> Ht.htmlDocumentToEventTarget
  offset <- getElementOffset
  let
    evntify :: forall a. a -> { clientX :: Number, clientY :: Number }
    evntify = Unsafe.Coerce.unsafeCoerce
    attachMouseUp f =
      Etr.addEventListener Etp.mouseup (Etr.eventListener f) false docTarget
    attachMouseMove f =
      Etr.addEventListener Etp.mousemove (Etr.eventListener f) false docTarget
    attachAnimated f =
      Etr.addEventListener Etp.animationend (Etr.eventListener f) false
      $ Ht.htmlElementToEventTarget el
    handleMouseUp e =
      pure $ action $ StopDragging
    handleMouseMove e =
      pure $ action $ ChangePosition (evntify e).clientX
    handleAnimated e =
      pure $ action $ Animated
  subscribe $ eventSource attachMouseUp handleMouseUp
  subscribe $ eventSource attachMouseMove handleMouseMove
  subscribe $ eventSource attachAnimated handleAnimated

  modify $ _constStyles .~ do
    (wrapperSelector key) ? do
      width $ px 400.0
      height $ px 50.0
      marginLeft $ px (-200.0)
      border solid (px 1.0) black
      overflow hidden
      position relative
      left $ pct 50.0
      top $ px 30.0
      padding (px 10.0) (px zero) (px 10.0) (px zero)
    (draggedSelector key) ? do
      position relative
      left $ px (-1700.0)
      marginLeft $ px 0.0
      width $ px 3000.0
    (itemSelector key) ? do
      width $ px 200.0
      display inlineBlock
      textAlign center

  pure next
eval (StartDragging startedAt next) = do
  mL <- getCurrentX
  offset <- getElementOffset
  modify $ _visualState .~ (Dragging $ startedAt - mL)
  modify $ _position .~ startedAt
  modify updateStyles
  pure next
eval (StopDragging next) = do
  dragged <- gets isDragged
  when dragged do
    mL <- getCurrentX
    Dragging startedAt <- gets _.visualState
    position <- gets _.position
    let
      diff = position - startedAt
      pos =
        Int.floor
        $ (if diff > 0.0
           then 100.0
           else -100.0) + diff
    modify $ _visualState .~ (Animating mL ((Int.toNumber (pos / 200) * 200.0)))
    modify updateStyles
  pure next
eval (ChangePosition pos next) = do
  dragged <- gets isDragged
  when dragged do
    modify (_position .~ pos)
    modify updateStyles
  pure next
eval (Animated next) = do
  state <- get
  modify $ _visualState .~ Staying
  curX <- getCurrentX
  modify $ _position .~ curX
  let
    items = liftNonEmpty (shift (-1 * (Int.floor (curX / 200.0)))) state.items
  setDisplayedItems items
  modify $ _items .~ items
  modify $ _position .~ zero
  modify updateStyles
  subscribe
    $ EventSource
    $ SCR.producerToStallingProducer
    $ produce \emit -> do
      emit $ E.Left $ action $ Selected $ Ne.head items
      emit $ E.Right unit
  pure next
