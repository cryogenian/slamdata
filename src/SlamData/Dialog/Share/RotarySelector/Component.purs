module SlamData.Dialog.Share.RotarySelector.Component where

import Prelude

import Control.Bind (ifM)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random, RANDOM())

import Data.Array as Arr
import Data.Functor (($>))
import Data.Functor.Eff (liftEff)
import Data.Lens (LensP(), lens, (%~), (.~), (?~))
import Data.Maybe as M
import Data.NonEmpty ((:|))
import Data.Date (nowEpochMilliseconds, Now())
import Data.Time (Milliseconds(..))
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

-- TODO: switch to tuple, since it has ring instances already
type Position =
  {
    x :: Number
  , y :: Number
  }

positionFromMouseEvent
  :: forall r
   . {clientX :: Number, clientY :: Number |r}
  -> Position
positionFromMouseEvent {clientX, clientY} =
  {
    x: clientX
  , y: clientY
  }

type State a =
  {
    initialPosition :: M.Maybe Position
  , currentPosition :: M.Maybe Position
  , lastPosition :: M.Maybe Position
  , select :: Select a
  , element :: M.Maybe Ht.HTMLElement
  , styles :: CSS
  , key :: M.Maybe String
  , animated :: Boolean
  , animationEndStyles :: CSS
  }

initialState :: forall a. State a
initialState =
  {
    initialPosition: M.Nothing
  , currentPosition: M.Nothing
  , lastPosition: M.Nothing
  , select: emptySelect
  , element: M.Nothing
  , styles: pure unit
  , key: M.Nothing
  , animated: false
  , animationEndStyles: pure unit
  }

_initialPosition :: forall a r. LensP {initialPosition :: a |r} a
_initialPosition = lens _.initialPosition _{initialPosition = _}

_currentPosition :: forall a r. LensP {currentPosition :: a |r} a
_currentPosition = lens _.currentPosition _{currentPosition = _}

_lastPosition :: forall a r. LensP {lastPosition :: a|r} a
_lastPosition = lens _.lastPosition _{lastPosition = _}

_select :: forall a r. LensP {select :: a|r} a
_select = lens _.select _{select = _}

_element :: forall a r. LensP {element :: a|r} a
_element = lens _.element _{element = _}

_styles :: forall a r. LensP {styles :: a|r} a
_styles = lens _.styles _{styles = _}

_key :: forall a r. LensP {key :: a|r} a
_key = lens _.key _{key = _}

_animated :: forall a r. LensP {animated :: a |r} a
_animated = lens _.animated _{animated = _}

_animationEndStyles :: forall a r. LensP {animationEndStyles :: a |r} a
_animationEndStyles = lens _.animationEndStyles _{animationEndStyles = _}

data Query a
  = Init Ht.HTMLElement a
  | StartDragging Position a
  | StopDragging Position a
  | DragCoords Position a
  | SetStyles CSS a
  | Transionend a

type RotarySelectorDSL a = ComponentDSL (State a) Query Slam

comp :: forall a. Component (State a) Query Slam
comp = component render eval

render :: forall a. State a -> ComponentHTML Query
render state =
  H.div
    ([
       E.onMouseDown (\evt ->
                        E.preventDefault
                        $> (action $ StartDragging $ positionFromMouseEvent evt))
     , P.initializer (\el -> action $ Init el)
    ]
     <> M.maybe [] (dataRotaryKey >>> Arr.singleton) state.key
    )
    ( stls
      <>
      [
        printState state
      ]
    )
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
    (selector k) ? do

      state.styles

  selector k =
    Selector (Refinement [ AttrVal "data-rotarykey" k ]) Star

hole :: forall a. a
hole = Unsafe.Coerce.unsafeCoerce unit

-- TODO: remove this crap
printState :: forall a. State a -> ComponentHTML Query
printState state =
  H.div_
    [
      H.p_ [ H.text $ show
             $ sub
               (state.currentPosition <#> _.x)
               (state.initialPosition <#> _.x)
           ]
    , H.p_ [ H.text "initialPosition" ]
    , H.p_ [ H.text $ show $ state.initialPosition <#> _.x ]
    , H.p_ [ H.text $ show $ state.initialPosition <#> _.y ]
    , H.p_ [ H.text "lastPosition" ]
    , H.p_ [ H.text $ show $ state.lastPosition <#> _.x ]
    , H.p_ [ H.text $ show $ state.lastPosition <#> _.x ]
    , H.p_ [ H.text "currentPosition" ]
    , H.p_ [ H.text $ show $ state.currentPosition <#> _.x ]
    , H.p_ [ H.text $ show $ state.currentPosition <#> _.y ]
    ]

eval :: forall a. Natural Query (RotarySelectorDSL a)
eval (StartDragging pos next) = do
  modify (_styles .~ pure unit)
  modify (_initialPosition ?~ pos)
  pure next
eval (StopDragging pos next) = do
  state <- get
  if state.animated
    then do
    modify $ _animated .~ false
    modify $ _styles .~ state.animationEndStyles
    modify $ _animationEndStyles .~ pure unit
    modify $ _currentPosition .~ M.Nothing
    modify $ _lastPosition .~ M.Nothing
    else do
    let
      lp =
        M.fromMaybe {x: 0.0, y: 0.0} state.lastPosition
      diff =
        M.fromMaybe {x: 0.0, y: 0.0}
        $ {x: _, y: _}
        <$> ((state.currentPosition <#> _.x) - (state.initialPosition <#> _.x))
        <*> ((state.currentPosition <#> _.y) - (state.initialPosition <#> _.y))
      new =
        { x: lp.x + diff.x, y: lp.y + diff.y }
    modify (_initialPosition .~ M.Nothing)
    modify (_lastPosition ?~ new)
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
  pure next
eval (Init el next) = do
  modify (_element ?~ el)
  k <- liftEff genKey
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
    attachTransitionend f =
      Etr.addEventListener Etp.animationend (Etr.eventListener f) false
      $ Ht.htmlElementToEventTarget el
    handleMouseUp e =
      pure $ action $ StopDragging $ positionFromMouseEvent $ evntify e
    handleMouseMove e =
      pure $ action $ DragCoords $ positionFromMouseEvent $ evntify e
    handleTransitionend e = do
      pure $ action $ Transionend

  subscribe $ eventSource attachMouseUp handleMouseUp
  subscribe $ eventSource attachMouseMove handleMouseMove
  subscribe $ eventSource attachTransitionend handleTransitionend
  pure next

eval (DragCoords pos next) = do
  state <- get
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
  pure next

eval (SetStyles css next) = modify (_styles .~ css) $> next
eval (Transionend next) = do
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
  pure next

-- | Generate unique key for component
genKey :: forall eff. Eff (now :: Now, random :: RANDOM | eff) String
genKey = do
  rn1 <- random
  rn2 <- random
  (Milliseconds time) <- nowEpochMilliseconds
  pure $ show rn1 <> show time <> show rn2


dataRotaryKey :: forall i r. String -> IProp r i
dataRotaryKey = Unsafe.Coerce.unsafeCoerce nonIndexed
  where
  nonIndexed :: String -> Prop i
  nonIndexed = Attr M.Nothing (attrName "data-rotarykey")

foreign import getComputedStyle
  :: forall e
   . Ht.HTMLElement
  -> Eff (dom :: DOM|e) (Sm.StrMap String)


foreign import toString
  :: forall e
   . Int
  -> Number
  -> String

randomString
  :: forall e
   . Eff (random :: RANDOM|e) String
randomString =
  random
    <#> toString 36
    <#> Rgx.replace numAndDot ""
  where
  numAndDot = Rgx.regex "(\\d|\\.)" Rgx.noFlags{global=true}
