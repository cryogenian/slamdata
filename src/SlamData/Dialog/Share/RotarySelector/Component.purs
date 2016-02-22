module SlamData.Dialog.Share.RotarySelector.Component
  ( comp
  , rotarySelector
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
import Data.ExistsR as Er

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

draggableScreens :: Int
draggableScreens = 3

type RotarySelectorConfig r =
  {
    itemRender :: M.Maybe (Option r -> ComponentHTML Query)
  , itemWidth :: Number
  , visibleItemCount :: M.Maybe Number
  , items :: Ne.NonEmpty Array (Option r)
  }

comp
  :: forall r
   . RotarySelectorConfig r
  -> { component :: Component State Query Slam
     , unpack :: OptionR -> Option r
     }
comp cfg =
  { component: component (render cfg unpack) (eval cfg)
  , unpack: Unsafe.Coerce.unsafeCoerce
  }
  where
  unpack :: OptionR -> Option r
  unpack = Unsafe.Coerce.unsafeCoerce

rotarySelector
  :: forall r p e
   . p
  -> RotarySelectorConfig r
  -> Array (Option r)
  -> { slot :: SlotConstructor State Query Slam p
     , unpack :: OptionR -> Option r
     }
rotarySelector p cfg items =
  { slot: SlotConstructor p \_ ->
      { component: rComp.component
      , initialState: initialState cfg.items
      }
  , unpack: rComp.unpack
  }
  where
  rComp = comp cfg

render
  :: forall r
   . RotarySelectorConfig r
  -> (OptionR -> Option r)
  -> State
  -> ComponentHTML Query
render cfg unpack state =
  H.div wrapperAttrs
    [ H.div [ E.onMouseDown (\evt -> E.preventDefault
                                       $> (action $ StartDragging evt.clientX))
            , P.initializer (\el -> action $ Init el)
            , P.classes [ draggedClass ]
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

  itemRender :: OptionR -> ComponentHTML Query
  itemRender s =
    H.div [ P.classes [ itemClass ] ]
      $ pure
      $ case cfg.itemRender of
        M.Just fn -> fn $ unpack s
        M.Nothing -> Er.runExistsR (runOption >>> _.label >>> H.text) s

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

setDisplayedItems
  :: forall r
   . RotarySelectorConfig r
  -> Ne.NonEmpty Array OptionR
  -> RotarySelectorDSL Unit
setDisplayedItems cfg arr = do
  screenWidth <- liftEff $ Br.getScreen <#> _.width
  let
    iwidth = Int.floor cfg.itemWidth
    ilen = Arr.length $ Ne.oneOf arr
    itemsOnScreen = screenWidth / iwidth + one
    repeats = draggableScreens * itemsOnScreen / ilen
  modify $ _displayedItems .~ liftNonEmpty (repeat repeats) arr


eval
  :: forall r
   . RotarySelectorConfig r
  -> Natural Query RotarySelectorDSL
eval cfg (Init el next) = do
  modify $ _element ?~ el
  key <- genKey
  modify $ _key ?~ key
  state <- get
  setDisplayedItems cfg state.items
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


  screenWidth <- liftEff $ Br.getScreen <#> _.width
  modify $ _constStyles .~ do
    let visibleCount = M.fromMaybe 2.0 cfg.visibleItemCount
    (wrapperSelector key) ? do
      width $ px $ visibleCount * cfg.itemWidth
      height $ px 50.0
      marginLeft $ px (-1.0 * visibleCount * cfg.itemWidth * 0.5)
      border solid (px 1.0) black
      overflow hidden
      position relative
      left $ pct 50.0
      top $ px 30.0
      padding (px 10.0) (px zero) (px 10.0) (px zero)
    (draggedSelector key) ? do
      let
        iwidth = Int.floor cfg.itemWidth
        ilen = Arr.length $ Ne.oneOf state.items
        itemsOnScreen = screenWidth / iwidth + one
        draggedWidth =
          cfg.itemWidth * Int.toNumber (itemsOnScreen * draggableScreens)
        itemsOnLeftSideCount =
          draggableScreens * (itemsOnScreen / 2 / ilen)
        halfWidth =
          cfg.itemWidth * Int.toNumber (ilen * itemsOnLeftSideCount)
        leftPosition =
          (visibleCount - one) / 2.0 * cfg.itemWidth - halfWidth
      position relative
      left $ px leftPosition
      marginLeft $ px 0.0
      width $ px $ draggedWidth
    (itemSelector key) ? do
      width $ px cfg.itemWidth
      display inlineBlock
      textAlign center

  pure next
eval cfg (StartDragging startedAt next) = do
  mL <- getCurrentX
  offset <- getElementOffset
  modify $ _visualState .~ (Dragging $ startedAt - mL)
  modify $ _position .~ startedAt
  modify updateStyles
  pure next
eval cfg (StopDragging next) = do
  visualState <- gets _.visualState
  case visualState of
    Dragging startedAt -> do
      mL <- getCurrentX
      position <- gets _.position
      let
        diff = position - startedAt
        pos =
          Int.floor
          $ ((if diff > 0.0 then 1.0 else -1.0) * cfg.itemWidth * 0.5)
          + diff
        finalMargin =
          Int.toNumber (pos / Int.floor cfg.itemWidth)  * cfg.itemWidth

      modify $ _visualState .~ Animating mL finalMargin
      modify updateStyles
    _ -> pure unit
  pure next
eval cfg (ChangePosition pos next) = do
  dragged <- gets isDragged
  when dragged do
    modify $ _position .~ pos
    modify updateStyles
  pure next
eval cfg (Animated next) = do
  state <- get
  modify $ _visualState .~ Staying
  curX <- getCurrentX
  modify $ _position .~ curX
  let
    items =
      liftNonEmpty (shift (-1 * (Int.floor (curX / cfg.itemWidth)))) state.items
  setDisplayedItems cfg items
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
eval cfg (GetSelected continue) = do
  state <- get
  pure $ continue $ Ne.head state.items
