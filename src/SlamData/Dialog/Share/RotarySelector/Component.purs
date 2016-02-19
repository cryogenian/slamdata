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
     <> M.maybe [] (dataRotaryKey >>> Arr.singleton) state.key
    )
    ( stls <> [ H.p_ [ H.text "drag me" ] ])
  where
  stls :: Array (ComponentHTML Query)
  stls =
    case state.key of
      M.Nothing ->
        []
      M.Just k ->
        [
          CSS.stylesheet (mkStylesheet k)
        ]

  mkStylesheet k =
    (selector k) ? state.styles

  selector k =
    Selector (Refinement [ AttrVal "data-rotarykey" k ]) Star


eval :: Natural Query RotarySelectorDSL
eval (Init el next) = do
  modify (_element ?~ el)
  k <- genKey
  modify (_key ?~ k)
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
--    attachTransitionend f =
--      Etr.addEventListener Etp.animationend (Etr.eventListener f) false
--      $ Ht.htmlElementToEventTarget el
    handleMouseUp e =
      pure $ action $ StopDragging -- $ (evntify e).clientX
    handleMouseMove e =
      pure $ action $ ChangePosition $ (evntify e).clientX
--    handleTransitionend e = do
--      pure $ action $ Transionend

  subscribe $ eventSource attachMouseUp handleMouseUp
  subscribe $ eventSource attachMouseMove handleMouseMove
--  subscribe $ eventSource attachTransitionend handleTransitionend
  pure next
eval (StartDragging startedAt next) = do
  mL <- M.fromMaybe zero <$> Mt.runMaybeT do
    el <- Mt.MaybeT $ gets _.element
    st <- liftEff $ getComputedStyle el
    Mt.MaybeT
      $ pure
      $ Sm.lookup "marginLeft" st
      <#> Global.readFloat
  modify $ _visualState .~ (Dragging $ startedAt - mL)
  pure next
eval (StopDragging next) = do
  modify $ _visualState .~ Staying
{-
    kfsKey <- liftEff randomString
    let
      aes = marginLeft $ px zero
      from = do
        marginLeft
        $ px
        $ add (M.fromMaybe zero (state.lastPosition <#> _.x))
        $ M.fromMaybe zero
        $ sub
        (state.currentPosition <#> _.x)
        (state.initialPosition <#> _.x)
      animationStyles = do
        keyframesFromTo kfsKey from aes
        animation
          (fromString kfsKey)
          (sec 1.0)
          easeOut
          (sec zero)
          (iterationCount one)
          normalAnimationDirection
          forwards
    modify (_animated .~ true)
    modify (_animationEndStyles .~ aes)
    modify (_styles .~ animationStyles)
-}
  pure next


eval (ChangePosition pos next) = do
  dragged <- gets isDragged
  when dragged do
    modify (_position .~ pos)
    modify updateStyles
{-  state <- get
  let
    styles =
      marginLeft
      $ px
      $ add (M.fromMaybe zero (state.lastPosition <#> _.x))
      $ M.fromMaybe zero
      $ sub
      (state.currentPosition <#> _.x)
      (state.initialPosition <#> _.x)
  modify (_currentPosition ?~ pos)
  modify (_styles .~ styles)
-}
  pure next
eval (Animated next) = do
  {-
  gets _.element >>= F.traverse_ \el -> do
    styles <- liftEff $ getComputedStyle el
    let
      pos =
        {
          x: M.fromMaybe zero $ Global.readFloat <$> Sm.lookup "marginLeft" styles
        , y: M.fromMaybe zero $ Global.readFloat <$> Sm.lookup "marginTop" styles
        }
    modify $ _lastPosition ?~ pos
  modify $ _styles .~ (pure unit)
-}
  pure next
