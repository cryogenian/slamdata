module SlamData.Dialog.Share.RotarySelector.Component
  ( comp
  , module SlamData.Dialog.Share.RotarySelector.Component.State
  , module SlamData.Dialog.Share.RotarySelector.Component.Query
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random, RANDOM())
import Control.Monad.Trans (lift)
import Control.Monad.Maybe.Trans as Mt
import Control.UI.Browser as Br

import Data.Array as Arr
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

import CSS.Geometry (height, width, left, marginLeft)
import CSS.Size (px)
import CSS.Stylesheet (CSS(), keyframesFromTo, (?))
import CSS.Animation (animation, iterationCount, normalAnimationDirection, forwards)
import CSS.Time (sec)
import CSS.Transition (easeOut)
import CSS.String (fromString)
import CSS.Render as Cr
import CSS.Selector (Selector(..), Predicate(AttrVal), Path(..), Refinement(..))

import Halogen hiding (Prop())
import Halogen.HTML.Core (Prop(..), attrName, className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties.Indexed (IProp())


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

type RotarySelectorDSL = ComponentDSL State Query Slam

dataRotaryKey :: forall i r. String -> IProp r i
dataRotaryKey = Unsafe.Coerce.unsafeCoerce nonIndexed
  where
  nonIndexed :: String -> Prop i
  nonIndexed = Attr M.Nothing (attrName "data-rotarykey")

comp :: forall a. Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.div [ P.classes [ className "rotary-selector" ] ]
    [ H.div
      ([
         E.onMouseDown (\evt -> E.preventDefault
                                 $> (action $ StartDragging evt.clientX))
       , P.initializer (\el -> action $ Init el)
       ]
       <> F.foldMap (dataRotaryKey >>> Arr.singleton) state.key
      )
      ( stls <> content )
    ]
  where
  content :: Array (ComponentHTML Query)
  content =
    map (H.div_ <<< Arr.singleton <<< H.text) state.displayedItems

  stls :: Array (ComponentHTML Query)
  stls =
    F.foldMap (Arr.singleton <<< CSS.stylesheet <<< mkStylesheet) state.key

  mkStylesheet :: String -> CSS
  mkStylesheet k =
    (selector k) ? state.styles

  selector :: String -> Selector
  selector k =
    Selector (Refinement [ AttrVal "data-rotarykey" k ]) Star

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

setDisplayedItems :: Array String -> RotarySelectorDSL Unit
setDisplayedItems arr = do
  screenWidth <- liftEff $ Br.getScreen <#> _.width
  let
    displayedItems =
      if Arr.null arr
      then [ ]
      else
        flip repeat arr
        $ Int.ceil
        $ (Int.toNumber screenWidth / 200.0 * 2.0)
        / (Int.toNumber (Arr.length arr))
  modify (_displayedItems .~ displayedItems)

eval :: Natural Query RotarySelectorDSL
eval (Init el next) = do
  state <- get
  modify (_element ?~ el)
  genKey >>= pure >>> L.set _key >>> modify
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
    items = shift (-1 * (Int.floor (curX / 200.0))) state.items
  setDisplayedItems items
  modify $ _items .~ items
  modify $ _position .~ zero
  modify updateStyles
  pure next
