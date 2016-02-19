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
import Halogen.HTML.Core (Prop(..), attrName)
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
import Utils.DOM (getComputedStyle)

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
  H.div
    ([
       E.onMouseDown (\evt -> E.preventDefault
                              $> (action $ StartDragging evt.clientX))
     , P.initializer (\el -> action $ Init el)
    ]
     <> F.foldMap (dataRotaryKey >>> Arr.singleton) state.key
    )
    ( stls <> [ H.p_ [ H.text "drag me" ] ])
  where
  stls :: Array (ComponentHTML Query)
  stls =
    F.foldMap (Arr.singleton <<< CSS.stylesheet <<< mkStylesheet) state.key

  mkStylesheet :: String -> CSS
  mkStylesheet k =
    (selector k) ? state.styles

  selector :: String -> Selector
  selector k =
    Selector (Refinement [ AttrVal "data-rotarykey" k ]) Star

getCurrentX :: RotarySelectorDSL Number
getCurrentX =
  M.fromMaybe zero <$> Mt.runMaybeT do
    el <- Mt.MaybeT $ gets _.element
    st <- liftEff $ getComputedStyle el
    Mt.MaybeT
      $ pure
      $ Sm.lookup "marginLeft" st
      <#> Global.readFloat

eval :: Natural Query RotarySelectorDSL
eval (Init el next) = do
  modify (_element ?~ el)
  genKey >>= pure >>> L.set _key >>> modify
  docTarget <-
    liftEff
    $ window
    >>= Win.document
    <#> Ht.htmlDocumentToEventTarget
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
      pure $ action $ ChangePosition $ (evntify e).clientX
    handleAnimated e =
      pure $ action $ Animated

  subscribe $ eventSource attachMouseUp handleMouseUp
  subscribe $ eventSource attachMouseMove handleMouseMove
  subscribe $ eventSource attachAnimated handleAnimated
  pure next
eval (StartDragging startedAt next) = do
  mL <- getCurrentX
  modify $ _visualState .~ (Dragging $ startedAt - mL)
  modify updateStyles
  pure next
eval (StopDragging next) = do
  mL <- getCurrentX
  modify $ _visualState .~ Animating mL zero
  modify updateStyles
  pure next
eval (ChangePosition pos next) = do
  dragged <- gets isDragged
  when dragged do
    modify (_position .~ pos)
    modify updateStyles
  pure next
eval (Animated next) = do
  modify $ _visualState .~ Staying
  getCurrentX >>= L.set _position >>> modify
  pure next
